"use strict";

function queryKunyomi() {
    var wrd = document.getElementById("textInput").value;

    fetch('/api/kunyomi/' + wrd)
    .then(function(response) {
        return response.json();
    })
    .then(function(res) {
        var spanCont = "";
        if (res.length == 0)
            spanCont += "<p>Cannot find the result for " + wrd + ".</p>";
        else {
            spanCont += "<p>Result:</p><ul>";
            for (var i = 0; i < res.length; i++)
                spanCont += "<li>" + res[i] + "</li>";
            spanCont += "</ul>";
        }
        document.getElementById("span_res").innerHTML = spanCont;
    });
}
