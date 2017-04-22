
/* A simple cache of JSON files */

var www_cache = {};

function download_json(url, callback)
{
    try {
        var json =  www_cache(url);
        if( json == null ) {
            really_download_json(url, callback);
        } else {
            callback(json);
        }
    } catch(err){
        really_download_json(url, callback);
    }
}

function really_download_json(url, callback){
    var xmlhttp = new XMLHttpRequest();
    
    console.log("download_json " + url);
    xmlhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            var myArr = JSON.parse(this.responseText);
            www_cache[url] = myArr;
            callback(myArr);
        }
    };
    xmlhttp.open("GET", url, true);
    xmlhttp.send();
}

function clear_cache()
{
    www_cache = {};
}

/* Some constants, never change */

var version_col = 0;
var switches_col = 1;
var first_row = 3;

/* fields that should be put without interpretation */
var json_package_fields = [
    "version_name",
    "status",
    "build_log",
    "build_result",
    "check_date",
    "switch_name",
    "commit_name",
    "build_info",
    "depends"
];

/* The state of the interface */

/* true: display table, false: display package */
var show_table = true;

var table_diff_mode = false;

/*
var table_json = "";
var table_switch = "";
var table_search = "";

var table_pos = 0;
var table_size = 100;

var package_json = "";

function object_of_state()
{
    return {
        show_table: show_table,
        
        table_json: table_json,
        table_switch: table_switch,
        table_search: table_search,
        table_diff_mode: table_diff_mode,
        table_pos: table_pos;
        table_size: table_size;

        package_json: package_json
    };
}

function history_update(){
    history.pushState( object_of_state(),
                       null,
                       "opam-builder.html");
}
*/


/* This function is used to display the initial page, after loading the page */

function body_onload()
{
    var search = document.getElementById("search");
    search.value = window.location.hash.substr(1);

    search.onkeyup = update_view;
    display_main_table();
}

/* Manage history */

window.addEventListener('popstate', function(e) {
    // e.state is equal to the data-attribute of the last image we clicked
    if( e.state == null ){
        console.log("popstate null !");

        display_main_table();
    } else {
        console.log("popstate " + e.state.message);
        if( e.state.table ){
            console.log("popstate to TABLE");
            if( e.state.switch_name == "" ){
                display_main_table();
            } else {
                display_switch_table(e.state.switch_name);
            }
            
        } else {
            console.log("popstate to PACKAGE");
            swap_to_package_json(e.state.json);
        }
    }
});



function switch_callback(sw)
{
    console.log("push history SWITCH " + sw);
    history.pushState( {
        message: "from switch_callback",
        table: true,
        switch_name: sw },
                       null, "opam-builder.html");
    display_switch_table(sw);
}
function display_switch_table(sw){
    load_table("Switch " + sw,
               "packages",
               sw + ".json",
               "main_callback" );
}

function main_callback()
{
    console.log("push history MAIN TABLE");
    history.pushState( {
        message: "from main_callback",
        table: true,
        switch_name: "" },
                       null, "opam-builder.html");
    display_main_table();
}

function display_main_table(){
    load_table("All Switches",
               "packages",
               "opam-builder.json",
               "switch_callback" );
}

function table_of_json(table_title, table_name, json, callback){
        var oldTable = document.getElementById(table_name),
            newTable = oldTable.cloneNode(true),
            tr, td;

        console.log("new json" + json.switches);

        var title = document.getElementById("table_title");
        title.innerHTML = table_title;

    clear_children(newTable);
        
        // row of switches
        tr = document.createElement('tr');
        td = document.createElement('td');  tr.appendChild(td);
        for(var i=0; i < json.commits.length; i++){
            td = document.createElement('td');
            var sw = json.switches[i];
            var a = document.createElement("a");
            var text = document.createTextNode(sw);
            a.href = "javascript:" + callback + "('" + sw + "')";
            a.appendChild(text);
            td.appendChild(a);
            tr.appendChild(td);
        }
        newTable.appendChild(tr);

        // row of dates
        tr = document.createElement('tr');
        td = document.createElement('td');  tr.appendChild(td);
        for(var i=0; i< json.dates.length; i++){
            td = document.createElement('td');
            var text = document.createTextNode(json.dates[i]);
            td.appendChild(text);
            tr.appendChild(td);
        }
        newTable.appendChild(tr);

        // row of commits
        tr = document.createElement('tr');
        td = document.createElement('td');  tr.appendChild(td);
        for(var i=0; i < json.commits.length; i++){
            td = document.createElement('td');
            var text = document.createTextNode(json.commits[i]);
            td.appendChild(text);
            tr.appendChild(td);
        }
        newTable.appendChild(tr);

        /* first row now */
        first_row = newTable.rows.length;
        
        for(var i=0; i < json.packages.length; i++){
            var p = json.packages[i];
            
            tr = document.createElement('tr');
            td = document.createElement('td');
            var text = document.createTextNode(p.p);
            td.appendChild(text);
            tr.appendChild(td);
            td = document.createElement('td');  tr.appendChild(td);
            newTable.appendChild(tr);
            
            for(var j=0; j < p.v.length; j++){
                var v = p.v[j];
                tr = document.createElement('tr');

                /* version */
                td = document.createElement('td');
                var text = document.createTextNode(v.v);
                td.appendChild(text);
                tr.appendChild(td);

                /* results per commit */
                for(var k=0; k < v.r.length; k++){
                    td = document.createElement('td');
                    var a = get_version_link(v.r[k],
                                             json.timestamps[k],
                                             json.commits[k],
                                             json.switches[k],
                                             v.v);
                    td.appendChild(a);
                    td.className = v.r[k];
                    tr.appendChild(td);
                }
                newTable.appendChild(tr);
            }
            
        }

        oldTable.parentNode.replaceChild(newTable, oldTable);
        newTable.style.display = "";
        document.getElementById("package").style.display = "none";

    update_view();
}

function update_main_table()
{
    clear_cache();
    main_callback();
}

function get_version_link(text,
                          timestamp_date, commit_name, switch_name,
                          version_name)
{
    var a = document.createElement('a');
    var text = document.createTextNode(text);
    
    /* we need to create a different url for every
       callback, so we must create a new scope. */
    a.href = "javascript:swap_to_package('" + 
        timestamp_date + '-' +
        commit_name + '-' +
        switch_name + '.files/' +
        version_name + ".json')";
    a.appendChild(text);
    return a;
}

function load_table(table_title, table_name, url, callback){
    download_json(url,
                  function(json) {
                      table_of_json(table_title, table_name, json, callback);
                  });
}

function swap_displays()
{
    show_table = !show_table;
    update_displays();
}

function update_displays()
{
    var table = document.getElementById("packages");
    var div = document.getElementById("package");
    if( show_table ){
        table.style.display = "";
        div.style.display = "none";
    } else {
        table.style.display = "none";
        div.style.display = "";
    }
}

function swap_to_package_json(json)
{
    var div;
    
    array_iter(json_package_fields,
                                 function(name){
                                     div = document.getElementById(name);
                                     div.innerHTML = json[name];
                                 });
    
    var depends = document.getElementById("depends");
    clear_children(depends);
    array_iter(json.depends,
               function(version_name){
                                     var li = document.createElement("li");
                   li.appendChild(
                       get_version_link(
                           version_name,
                           json.timestamp_date,
                           json.commit_name,
                           json.switch_name,
                           version_name)
                   );
                   depends.appendChild(li);
               });
    show_table = false;
    update_displays();
}

function swap_to_package(url)
{
    download_json(url, function(json){
        console.log("push history PACKAGE" + json.version_name);
        history.pushState(
            {
                message: "from swap_to_package",
                table: false,
                json: json },
                          null,
                          "#"+json.version_name);
        swap_to_package_json(json);
    });
}

function swap_diff()
{
    table_diff_mode = !table_diff_mode;
    update_view();
}

function clear_search()
{
    var search = document.getElementById("search");
    search.value = "";
    update_view();
}

function update_view()
{
    var table = document.getElementById("packages");
    var display = [];
    for(var i = first_row; i < table.rows.length; i++){
        display[i] = true;
    }

    /* Update from search */
    var search = document.getElementById("search");
    var pattern = search.value.toUpperCase();
    for(var i = first_row; i < table.rows.length; i++){
        var tr = table.rows[i];
        var td = tr.cells[version_col].innerHTML;
        if(td == "" || td.toUpperCase().indexOf(pattern) >= 0 ){
        } else {
            display[i] = false;
        }
    }

    /* Update from diff */
    if( table_diff_mode ){
        for(var i = first_row; i < table.rows.length; i++){
            var tr = table.rows[i];
            var class1 = tr.cells[1].className;
            var disp = false;
            for(var j=2; j<tr.cells.length; j++){
                var class2 = tr.cells[j].className;
                if( class1 != class2 ) disp = true;
            }
            if(!disp) display[i] = false;
        }
    }
    
    for(var i = first_row; i < table.rows.length; i++){
        var tr = table.rows[i];
        if( display[i] ){
            tr.style.display = "";
        } else {
            tr.style.display = "none";
        }
    }
    
}



/* These functions are the ones appearing in opam-builder.html */

function button_clear_search()
{
    clear_search();
}
function button_update_all_switches()
{
    update_main_table();
}
function button_swap_view_switches_package()
{
    swap_displays();
}
function button_swap_diff_mode()
{
    swap_diff();
}
function button_clear_cache()
{
    clear_cache();
}

/* Generic functions */

function array_iter(table, f){
    for(var i = 0; i < table.length; i++){
        f(table[i]);
    }
}

function clear_children(t){
    while (t.hasChildNodes()) {
        t.removeChild(t.lastChild);
    }
}
