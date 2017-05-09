
/* From stackoverflow, code to parse arguments */

var urlParams;
(window.onpopstate = function () {
    var match,
        pl     = /\+/g,  // Regex for replacing addition symbol with a space
        search = /([^&=]+)=?([^&]*)/g,
        decode = function (s) {
            return decodeURIComponent(s.replace(pl, " ")); },
        query  = window.location.search.substring(1);

    console.log("query  = " + window.location.search.substring(1) );
    console.log("search = " +  window.location.pathname );

    urlParams = {};
    while (match = search.exec(query))
       urlParams[decode(match[1])] = decode(match[2]);
})();

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
    
    console.log("download_json [" + url + "]");
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

var initial_json_url = "opam-builder.json";
var initial_page_size = 200;

var version_col = 0;
var switches_col = 1;
var first_row = 3;


var index_table_id = "id-index-table";
var packages_table_id = "id-packages-table";
var search_id = "id-search";
var table_title_id = "id-table-title";
var package_id = "id-package";
var packages_id = "id-packages";
var index_title_id = "id-index-title";
var diff_mode_id = "id-diff-mode";
var depends_id = "id-depends";
var select_page_size_id = 'id-select-page-size';
var select_table_tag_id = 'id-select-table-tag';

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

/* 0: display table, 1: display package */
var show_table = -1;

var table_json_url = ""; // url of json file
var table_json = {};     // content of json file
var table_table = [];    // table extracted and filtered
var table_index = [];
var table_index_size = -1;

var table_diff_mode = -1;
var table_page = 0;       // position of displayed table
var table_page_size = initial_page_size;    // min number of displayed entries
var table_title = "";   // current displayed switch
var table_search = "";   // global search
var table_tag = "";      // Ok, Missing, Fail, BadDeps

var package_json_url = "";
var package_json = null;
var package_updated = false;

function object_of_state()
{
    return {
        show_table: show_table,
        
        table_json_url: table_json_url,
        table_diff_mode: table_diff_mode,
        table_page: table_page,
        table_page_size: table_page_size,
        table_title: table_title,
        table_search: table_search,
        table_tag: table_tag,
        
        package_json_url: package_json_url,
    };
}

function parseInt_or_default( s, safe )
{
    var n = parseInt(s);
    if( isNaN(n) ){
        console.log("parseInt("+s+") returned NaN");
        return safe;
    }
    return n;
}

function update_state_from_url( s )
{
    if( urlParams[ "switch" ] != null ){
        s.table_json_url = urlParams[ "switch" ];
    }
    
    if( urlParams[ "q" ] != null ){
        s.table_search = urlParams[ "q" ];
    }

    if( urlParams[ "p" ] != null ){
        s.package_json_url = urlParams[ "p" ];
    }

    if( urlParams[ "view" ] != null ){
        s.show_table = parseInt_or_default(urlParams[ "view" ],
                                           s.show_table);
    }

    if( urlParams[ "size" ] != null ){
        s.table_page_size = parseInt_or_default(urlParams[ "size" ],
                                               s.table_page_size);
    }

    if( urlParams[ "page" ] != null ){
        s.table_page = parseInt(urlParams[ "page" ],
                                s.table_page);
    }

    if( urlParams[ "tag" ] != null ){
        s.table_tag = urlParams[ "tag" ];
    }

    if( urlParams[ "diff" ] != null ){
        s.table_diff_mode = parseInt(urlParams[ "diff" ],
                                     s.table_diff_mode);
    }
    
}

function add_url_argument(arg, value){

    console.log("add_url_argument " + arg + " " + value);
    return (
        encodeURIComponent(arg) + "=" +
            encodeURIComponent( ""+ value ) + "&");
}

function update_url_from_state( url, s )
{
    url += "?";
    if( s.show_table == 0 ){

        if ( ! (s.table_json_url === initial_json_url) ){
            url += add_url_argument("switch", s.table_json_url );
        }
        if( ! (s.table_search === "") ){
            url += add_url_argument("q", s.table_search );            
        }
        if( ! (s.table_page === 0) ){
            url += add_url_argument("page", s.table_page );
        }
        if( ! (s.table_page_size === initial_page_size) ){
            url += add_url_argument("size", s.table_page_size );
        }
        if( ! (s.table_diff_mode === 0) ){
            url += add_url_argument("diff", s.table_diff_mode );
        }
        if( ! (s.table_tag === "") ){
            url += add_url_argument("tag", s.table_tag );
        }

    } else {
        
        url += add_url_argument("p", s.package_json_url );
        url += add_url_argument("view", s.show_table );
    }
    
    
    return url;
}

/* Managemenet of history */

function history_update( s ){
    var url = update_url_from_state( window.location.pathname, s );
    history.pushState( s, null, url);
}

function history_init( s ){
    var url = update_url_from_state( window.location.pathname, s );
    history.replaceState( s, null, url );
}

/* This function is used to display the initial page, after loading the page */

function initial_state()
{
    return {
        show_table: 0,
        
        table_json_url: initial_json_url,
        table_diff_mode: 0,
        table_page: 0,
        table_page_size: initial_page_size,
        table_title: "All Switches",
        table_search: "",
        table_tag: "",
        
        package_json_url: ""
    };
}

function body_onload()
{
    console.log("body_onload");
    /*
    var search = document.getElementById(search_id);
    search.onkeyup = button_global_search;
*/
    var s = initial_state();

    update_state_from_url( s );
    history_init( s );
    update_interface( s );
    update_view( s );
}

function update_interface( s )
{
    var select = document.getElementById(select_table_tag_id);
    var options = select.options;
    for(var i = 0; i<options.length; i++){
        var option = options[i];
        if( s.table_tag === option.value ){
            options.selectedIndex = i;
        }
    }

    select = document.getElementById(select_page_size_id);
    options = select.options;
    for(var i = 0; i<options.length; i++){
        var option = options[i];
        if( s.table_page_size === parseInt(option.value) ){
            options.selectedIndex = i;
        }
    }

    var search = document.getElementById(search_id);
    search.value = s.table_search;
}

    
/* Manage history */

window.addEventListener('popstate', function(e) {
    // e.state is equal to the data-attribute of the last image we clicked
    console.log("popstate !");

    var s = e.state;
    if( s == null ){
        s = object_of_state();
        s.table_json_url = initial_json_url;
    }
    update_interface( s );
    update_view( s );
});


function page_update_view( s )
{
    console.log("page_update_view");
    history_update( s );
    update_view( s );
}

function update_view( s )
{
    console.log("update_view");
    var immediate_update = true;
    
    if( ! (s.table_json_url === table_json_url) ){

        immediate_update = false;
        download_json( s.table_json_url,
                       function(json){
                           table_json_url = s.table_json_url;
                           table_json = json;
                           table_table = []; /* force update */
                           update_view( s );
                       });
    }
    
    if( ! (s.package_json_url === package_json_url) &&
        ! (s.package_json_url === "") ){
        
        immediate_update = false;
        download_json( s.package_json_url,
                       function(json){
                           package_json_url = s.package_json_url;
                           package_json = json;
                           package_updated = false;
                           update_view( s );
                       });
        
    }

    if( immediate_update ){
        really_update_view( s );
    }
}

function row_has_diff(v)
{
    var r = v.r[0];
    for(var i = 1; i < v.r.length; i++){
        if( ! ( v.r[i] === r ) ) return true;
    }
    return false;;
}

function row_has_tag(v)
{
    for(var i = 0; i < v.r.length; i++){
        if( v.r[i] === table_tag ) return true;
    }
    return false;;
}

function update_table()
{
    console.log("update_table");
    
    if( table_search === "" && table_diff_mode === 0 && table_tag == ""){
        table_table = table_json.packages;
    } else {
        var table_num = 0;
        var p,v, i,j,k,pp,p_index,p_num;
        var pattern = table_search.toUpperCase();
        for(i=0; i < table_json.packages.length; i++){
            p = table_json.packages[i];
            p_index = -1;
            for(j=0; j < p.v.length; j++){
                v = p.v[j];
                if( v.v.toUpperCase().indexOf(pattern) >= 0 &&
                    ( table_diff_mode === 0 || row_has_diff(v) ) &&
                    ( table_tag === "" || row_has_tag(v))
                  ){
                    if( p_index < 0 ){
                        var name = p.p;
                        pp = {};
                        pp.p = name;
                        pp.v = [];
                        p_index = table_num;
                        table_table[p_index] = pp;
                        table_num++;
                        p_num = 0;
                    }
                    pp.v[ p_num ] = v;
                    p_num++;
                }
            }
        }
    }
}

var button_diff_mode_id = 'id-button-diff-mode';

/* called when table_json_url is ok */
function really_update_view( s )
{
    console.log("really_update_view");
    if( ! (s.table_search === table_search) ||
        ! (s.table_diff_mode === table_diff_mode) ||
        ! (s.table_tag === table_tag)
      ){
        table_diff_mode = s.table_diff_mode;
        table_tag = s.table_tag;
        var diff_mode_button = document.getElementById(button_diff_mode_id);
        if( ! (table_diff_mode === 0) ){
            diff_mode_button.innerHTML = 'Exit Diff Mode';
        } else {
            diff_mode_button.innerHTML = 'Switch to Diff Mode';
        }

        
        table_search = s.table_search;
        table_title = 'bad';
        table_table = [];
    }

    if( table_table.length == 0 ) {
        table_title = 'bad';
        table_page_size = -1;
        
        if( table_json[ "title" ] != null ){
            s.table_title = table_json[ "title" ];
        }

        update_table();
    }

    if( s.table_page_size != table_page_size ){
        table_page_size = s.table_page_size;
        table_page = -1;
        table_title = 'bad';
        update_index();
    }

    if( s.table_page != table_page ){
        table_page = s.table_page;
        
        if( table_page < 0 || table_page >= table_index.length ){
            table_page = 0;
        }
        
        update_packages_rows();
    }

    if( ! (s.table_title === table_title) ){
        table_title = s.table_title;
        var new_title = table_title;
        var title = document.getElementById(table_title_id);
        if( ! (table_diff_mode === 0) )
            new_title += " (diff mode)";
        if( ! (table_tag === "") )
            new_title += " (tag "+ table_tag + " )";
        if( ! ( table_search === "" ) )
            new_title += " (pattern [" + table_search + "] )";
        new_title += " (view " + table_page_size + " versions)";
        title.innerHTML = new_title;
    }

    if( package_json != null && !package_updated ){
        package_updated = true;
        display_package_json( package_json );
    }

    
    if( s.show_table != show_table ){
        show_table = s.show_table;
        var table = document.getElementById(packages_id);
        var div = document.getElementById(package_id);
        if( show_table == 0 ){
            table.style.display = "";
            div.style.display = "none";
        } else {
            table.style.display = "none";
            div.style.display = "";
        }
    }
    /*
    if( s.table_diff_mode != table_diff_mode ){
        table_diff_mode = s.table_diff_mode;
        var title = document.getElementById(diff_mode_id);
        if( table_diff_mode == 1 ){
            title.innerHTML = "(diff mode)";
        } else {
            title.innerHTML = "";
        }        
    }
*/
    if( show_table == 0 ){
        update_local_view();
    }
    
}

function update_index()
{
    console.log("update_index");
    table_index = [];
    table_index_size = 0;

    if( table_table.length > 0 ){
        var i = 0;
        var p = table_table[0];
        var page_size = 0;
        var index;
        var new_index = function(){
            index = {
                index_page: 1,
                index_name: p.p + " ... ",
                index_long_name: p.p + " ... ",
                index_begin: i,
                index_end: i
            };
            table_index[table_index_size] = index;
            table_index_size++;
            page_size = p.v.length;
        };
        var flush_index = function(){
            if( page_size >= table_page_size ){
                index.index_long_name += table_table[index.index_end].p;
                page_size = -1;
        }
        };
        new_index();
        for(i=1; i < table_table.length; i++){
            flush_index();
            p = table_table[i];
            if( page_size < 0 ) new_index();
            page_size += p.v.length;
            index.index_end = i;
        }
        index.index_long_name += table_table[index.index_end].p;
    }
    
    update_index_rows();
}

function update_index_rows()  
{
    console.log("update_index_rows");

    var oldTable = document.getElementById(index_table_id),
        newTable = oldTable.cloneNode(true),
        tr, td;

    clear_children(newTable);
    if( table_index.length > 1){
        for(var i=0; i < table_index.length; i++){
            tr = document.createElement('tr');
            td = document.createElement('td');
            var text = document.createTextNode(
                table_index[i].index_name);
            var a = document.createElement("a");
            a.appendChild(text);
            td.appendChild(a);
            a.href = "javascript:switch_to_page(" + i + ")";
            tr.appendChild(td);
            newTable.appendChild(tr);
        }
    }
    oldTable.parentNode.replaceChild(newTable, oldTable);
}

function update_packages_rows()  
{
    console.log("update_packages_rows");
    var oldTable = document.getElementById(packages_table_id),
        newTable = oldTable.cloneNode(true),
        tr, td;

    var title = document.getElementById(index_title_id);
    if( table_index.length > 0 ){
        var new_title = "";
        new_title += "Page " + table_page + ": ";
        new_title += table_index[table_page].index_long_name;
        title.innerHTML =  new_title;

    } else {
        title.innerHTML = "Global search gave no results for '" +
            table_search + "'";
    }
    
    clear_children(newTable);

    // row of switches
    tr = document.createElement('tr');
    td = document.createElement('td');  tr.appendChild(td);
    for(var i=0; i < table_json.commits.length; i++){
        td = document.createElement('td');
        var sw = table_json.switches[i];
        var a = document.createElement("a");
        var text = document.createTextNode(sw);
        a.href = "javascript:switch_callback('" + sw + "')";
        a.appendChild(text);
        td.appendChild(a);
        tr.appendChild(td);
    }
    newTable.appendChild(tr);
    
    // row of dates
    tr = document.createElement('tr');
    td = document.createElement('td');  tr.appendChild(td);
    for(var i=0; i< table_json.dates.length; i++){
        td = document.createElement('td');
        var text = document.createTextNode(table_json.dates[i]);
        td.appendChild(text);
        tr.appendChild(td);
    }
    newTable.appendChild(tr);
    
    // row of commits
    tr = document.createElement('tr');
    td = document.createElement('td');  tr.appendChild(td);
    for(var i=0; i < table_json.commits.length; i++){
        td = document.createElement('td');
        var text = document.createTextNode(table_json.commits[i]);
        td.appendChild(text);
        tr.appendChild(td);
    }
    newTable.appendChild(tr);
    
    /* first row now */
    first_row = newTable.rows.length;

    if( table_index.length > 0 ){
        for(var i=table_index[table_page].index_begin;
            i <= table_index[table_page].index_end;
            i++){
            var p = table_table[i];
            
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
                var a = document.createElement('a');
                var text = document.createTextNode(v.v);
                a.href = 'https://github.com/ocaml/opam-repository/tree/master/packages/' + p.p + '/' + v.v + '/opam';
                a.target = 'packages';
                a.appendChild(text);
                td.appendChild(a);
                tr.appendChild(td);
                
                /* results per commit */
                for(var k=0; k < v.r.length; k++){
                    td = document.createElement('td');
                    var a = get_version_link(v.r[k],
                                             table_json.timestamps[k],
                                             table_json.commits[k],
                                             table_json.switches[k],
                                             v.v);
                    td.appendChild(a);
                    td.className = v.r[k];
                    tr.appendChild(td);
                }
                newTable.appendChild(tr);
            }
        }
    }
    
    oldTable.parentNode.replaceChild(newTable, oldTable);
}

























function switch_callback(sw)
{
    var s = object_of_state();
    s.table_json_url = sw + ".json";
    s.table_title = "Switch " + sw;
    page_update_view( s );
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

function swap_displays()
{
    console.log("swap_displays");
    var s = object_of_state();
    s.show_table = 1 - show_table;
    page_update_view( s );
}

function display_package_json(json)
{
    console.log("display_package_json");
    var div;
    
    array_iter(json_package_fields,
               function(name){
                   div = document.getElementById("id-" + name);
                   div.innerHTML = json[name];
               });
    
    var depends = document.getElementById(depends_id);
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
}

function swap_to_package_old(url)
{
    console.log("swap_to_package");
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

function swap_to_package(url)
{
    var s = object_of_state();
    s.package_json_url = url;
    s.show_table = 1;
    page_update_view( s );
}

function swap_diff()
{
    console.log("swap_diff");
    var s = object_of_state();
    s.table_diff_mode = 1 - s.table_diff_mode;
    s.show_table = 0;
    page_update_view( s );
}

function clear_search()
{
    console.log("clear_search");
    var search = document.getElementById(search_id);
    search.value = "";
    var s = object_of_state();
    s.table_search = "";
    s.show_table = 0;
    page_update_view( s );
}

function update_local_view()
{
    console.log("update_local_view");
    var table = document.getElementById(packages_table_id);
    var display = [];
    for(var i = first_row; i < table.rows.length; i++){
        display[i] = true;
    }

    /* Update from search */
    var search = document.getElementById(search_id);
    var pattern = search.value.toUpperCase();
    for(var i = first_row; i < table.rows.length; i++){
        var tr = table.rows[i];
        var td = tr.cells[version_col].innerHTML;
        if(td === "" || td.toUpperCase().indexOf(pattern) >= 0 ){
        } else {
            display[i] = false;
        }
    }

    /* Update from diff 
    if( table_diff_mode == 1 ){
        for(var i = first_row; i < table.rows.length; i++){
            var tr = table.rows[i];
            var class1 = tr.cells[1].className;
            var disp = false;
            for(var j=2; j<tr.cells.length; j++){
                var class2 = tr.cells[j].className;
                if( ! (class1 === class2) ) disp = true;
            }
            if(!disp) display[i] = false;
        }
    }
    */
    
    for(var i = first_row; i < table.rows.length; i++){
        var tr = table.rows[i];
        if( display[i] ){
            tr.style.display = "";
        } else {
            tr.style.display = "none";
        }
    }
    
}

function switch_to_page(page)
{
    var s = object_of_state();
    s.table_page = page;
    page_update_view( s );
}


/* These functions are the ones appearing in opam-builder.html */

function button_clear_search()
{
    clear_search();
}
function button_all_switches()
{
    var s = object_of_state();
    s.table_json_url = initial_json_url;
    s.table_title = "All Switches";
    s.show_table = 0;
    page_update_view( s );
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

function button_page_next()
{
    if( table_page+1 < table_index.length ){
        var s = object_of_state();
        s.table_page = table_page+1;
        s.show_table = 0;
        page_update_view( s );
    }
}

function button_page_prev()
{
    if( table_page > 0 ){
        var s = object_of_state();
        s.table_page = table_page-1;
        s.show_table = 0;
        page_update_view( s );
    }
}

function button_global_search()
{
    var s = object_of_state();
    var search = document.getElementById(search_id);
    s.table_search = search.value;
    s.table_page = 0;
    page_update_view( s );
}

function select_table_tag()
{
    console.log("select_table_tag");
    var s = object_of_state();
    var select = document.getElementById(select_table_tag_id);
    var options = select.options;
    var option = options[options.selectedIndex];
    console.log("select_table_tag");
    s.table_tag = option.value;
    s.show_table = 0;
    page_update_view( s );
}

function select_page_size()
{
    var s = object_of_state();
    var select = document.getElementById(select_page_size_id);
    var options = select.options;
    var option = options[options.selectedIndex];
    s.table_page_size = parseInt(option.value);
    s.show_table = 0;
    page_update_view( s );
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

