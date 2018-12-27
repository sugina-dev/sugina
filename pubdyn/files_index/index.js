"use strict";

window.onload = function() {
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
                document.getElementById("h3_userinfo").innerHTML = "Welcome, " + res + ". <a href=\"/auth/logout\">Log Out</a>";
            }
        });

    fetch('/api/isadmin')
        .then(function(response) {
          return response.json();
        })
        .then(function(res) {
            if (res) {
                document.getElementById("span_contents_reserve").innerHTML = "<article>"
                    +   "<h3><a href=\"/pridyn/index.html\">Administrator Tools</a></h3>"
                    +   "<p>Administrator tools.</p>"
                    + "</article>";
            }
        });
}
