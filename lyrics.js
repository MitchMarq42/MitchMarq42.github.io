var r_text = [
    "quote # 1",
    "quote # 2",
    "quote # 3",
    "etc..."
];
function doText() {
    var i = Math.floor((r_text.length-1)*Math.random())
    document.getElementById(‘quotes’).innerHTML = r_text[i];
}
var myInterval = setInterval(doText, 5000);
