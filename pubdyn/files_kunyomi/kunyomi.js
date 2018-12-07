var httpRequest;

function queryKunyomi() {
    var wrd = document.getElementById("textInput").value;

    httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = handleResponse;
    httpRequest.open("GET", "/api/kunyomi/" + wrd, true);
    httpRequest.setRequestHeader("Content-type", "application/json");
    httpRequest.send();

    function handleResponse() {
        if (httpRequest.readyState == 4 && httpRequest.status == 200) {
            var res = JSON.parse(httpRequest.responseText);
            var spanCont = "";
            if (res.length == 0)
                spanCont += "<p>Cannot find the result for the designated word.</p>";
            else {
                spanCont += "<p>Result:</p><ul>";
                for (var i = 0; i < res.length; i++)
                    spanCont += "<li>" + res[i] + "</li>";
                spanCont += "</ul>";
            }
            document.getElementById("span_res").innerHTML = spanCont;
        }
    }
}
