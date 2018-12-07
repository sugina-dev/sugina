var httpRequest;

window.onload = function() {
    httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = handleResponse;
    httpRequest.open("GET", "/api/dictum", true);
    httpRequest.setRequestHeader("Content-type", "text/plain");
    httpRequest.send();

    function handleResponse() {
        if (httpRequest.readyState == 4 && httpRequest.status == 200)
            document.getElementById("p_dictum").innerText = httpRequest.responseText;
    }
}
