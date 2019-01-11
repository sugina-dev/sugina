"use strict";

function escapeHTML(html) {
    var p = document.createElement('p');
    p.innerText = html;
    return p.innerHTML;
}

function btn_post() {
    fetch('/api/board/message', {
        method: 'POST',
        body: JSON.stringify(document.getElementById("textarea_comment").value),
        headers: {
            'Content-Type': 'application/json'
        }
    })
    .then(function(response) {
        return response.json();
    })
    .then(function(res) {
        document.getElementById("textarea_comment").value = "";
        refreshBoard();
    });
}

function refreshBoard() {
    fetch('/api/board/message')
    .then(function(response) {
        return response.json();
    })
    .then(function(messages) {
        var content = "<table><th>Time</th><th>Message</th><th>Reply</th>";
        for (var i = 0; i < messages.length; i++) {
            content += "<tr>"
                + "<td>" + messages[i].time + "</td>"
                + "<td>" + escapeHTML(messages[i].message) + "</td>"
                + "<td>" + escapeHTML(messages[i].reply) + "</td>"
                + "</tr>";
        }
        content += "</table>";
        document.getElementById("div_table").innerHTML = content;
    });
}

document.addEventListener("DOMContentLoaded", function() {
    fetch('/api/username')
    .then(function(response) {
        return response.json();
    })
    .then(function(res) {
        if (!res) {
            document.body.innerHTML = "<h1>Please use this feature after logged in!</h1>";
        } else {
            refreshBoard();
        }
    });
});
