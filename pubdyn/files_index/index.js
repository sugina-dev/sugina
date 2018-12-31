"use strict";

document.addEventListener("DOMContentLoaded", function() {
    fetch('/api/dictum')
    .then(function(response) {
        return response.text();
    })
    .then(function(str) {
        document.getElementById("header_dictum").innerText = str;
    });

    fetch('/api/username')
    .then(function(response) {
        return response.json();
    })
    .then(function(res) {
        if (!res) {
            document.getElementById("h3_userinfo").innerHTML = "<a href=\"/auth/login\">Log In</a>";
        } else {
            document.getElementById("span_contents_useronly").style.display = "inherit";
            document.getElementById("h3_userinfo").innerHTML = "Welcome, " + res + ". <a href=\"/auth/logout\">Log Out</a>";
        }
    });

    fetch('/api/isadmin')
    .then(function(response) {
        return response.json();
    })
    .then(function(res) {
        if (res) {
            document.getElementById("span_contents_adminonly").style.display = "inherit";

            fetch('/pridyn/kakitsubata/index.csv')
            .then(function(response) {
                return response.text();
            })
            .then(function(str) {
                var lines = str.split('\n'), articles = "";
                for (var i = 0; i < lines.length; i++) {
                    var xs = lines[i].split(',');
                    if (xs.length == 2) {
                        articles += "<article>"
                            + "<h3><a href=\"/pridyn/kakitsubata/" + xs[0] + ".html\">" + xs[1] + "</a></h3>"
                            + "</article>";
                    }
                }
                document.getElementById("span_contents_adminonly_2").innerHTML += articles;
            });
        }
    });

    fetch('/pubdyn/offprint/index.csv')
    .then(function(response) {
        return response.text();
    })
    .then(function(str) {
        var lines = str.split('\n'), articles = "";
        for (var i = 0; i < lines.length; i++) {
            var xs = lines[i].split(',');
            if (xs.length == 2) {
                articles += "<article>"
                    + "<h3><a href=\"/pubdyn/offprint/" + xs[0] + ".html\">" + xs[1] + "</a></h3>"
                    + "</article>";
            }
        }
        document.getElementById("span_contents_pubdyn_offprint").innerHTML += articles;
    });
});
