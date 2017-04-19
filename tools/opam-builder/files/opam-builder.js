

function switch_callback(sw)
{
    load_table("Switch " + sw,
               "packages",
               sw + ".json",
               main_callback );
}

function main_callback()
{
    load_table("All Switches",
               "packages",
               "opam-builder.json",
               switch_callback );
}

function load_table(table_title, table_name, json_file, callback){
    
    var myFunction = function(json) {
        var oldTable = document.getElementById(table_name),
            newTable = oldTable.cloneNode(true),
            tr, td;

        console.log("new json" + json.switches);

        var title = document.getElementById("table_title");
        title.innerHTML = table_title;
        
        /* remove all previous children */
        while (newTable.hasChildNodes()) {
            newTable.removeChild(newTable.lastChild);
        }
        
        // row of switches
        tr = document.createElement('tr');
        td = document.createElement('td');  tr.appendChild(td);
        for(var i=0; i < json.commits.length; i++){
            td = document.createElement('td');
            var sw = json.switches[i];
            console.log(sw);
            var text = document.createTextNode(sw);
            td.onclick = function(){
                console.log("clicked");
                callback(sw);
            };
            td.appendChild(text);
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
        
        for(var i=0; i < json.packages.length; i++){
            var p = json.packages[i];
            for(var j=0; j < p.v.length; j++){
                var v = p.v[j];
                tr = document.createElement('tr');
                td = document.createElement('td');
                var text = document.createTextNode(v.v);
                td.appendChild(text);
                tr.appendChild(td);
                for(var k=0; k < v.r.length; k++){
                    td = document.createElement('td');
                    var text = document.createTextNode(v.r[k]);
                    td.className = v.r[k];
                    td.appendChild(text);
                    tr.appendChild(td);
                }
                newTable.appendChild(tr);
            }
            
        }

        console.log("replace new table...");
        oldTable.parentNode.replaceChild(newTable, oldTable);
        console.log("replace new table done.");
    }

    var xmlhttp = new XMLHttpRequest();
    var url = json_file;
        
    xmlhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            var myArr = JSON.parse(this.responseText);
            myFunction(myArr);
        }
    };
    xmlhttp.open("GET", url, true);
    xmlhttp.send();

}



function opam_onload()
{
    var search = document.getElementById("search");
    search.onkeyup = function(){
        var pattern = search.value.toUpperCase();
        var table = document.getElementById("packages");
        for(var i = 0; i < table.rows.length; i++){
            var tr = table.rows[i];
            var td = tr.cells[0].innerHTML;
            if(td.toUpperCase().indexOf(pattern) >= 0 ){
                tr.style.display = "";
            } else {
                tr.style.display = "none";
            }
        }
    };
    main_callback();
}
