var mousePosition;
var offset = [0,0];
var div;
var isDown = false;

div = document.getElementById("test");

div.addEventListener('mousedown', function(e) {
    isDown = true;
    offset = [
        div.offsetLeft - e.clientX,
        div.offsetTop - e.clientY
    ];
}, true);

div.addEventListener('mouseup', function() {
    isDown = false;
}, true);

div.addEventListener('mousemove', function(event) {
    event.preventDefault();
    if (isDown) {
        mousePosition = {
    
            x : event.clientX,
            y : event.clientY
    
        };
        div.style.left = (mousePosition.x + offset[0]) + 'px';
        div.style.top  = (mousePosition.y + offset[1]) + 'px';
    }
}, true);

div2 = document.getElementById("test2");

div2.addEventListener('mousedown', function(e) {
    isDown = true;
    offset = [
        div2.offsetLeft - e.clientX,
        div2.offsetTop - e.clientY
    ];
}, true);

div2.addEventListener('mouseup', function() {
    isDown = false;
}, true);

div2.addEventListener('mousemove', function(event) {
    event.preventDefault();
    if (isDown) {
        mousePosition = {
    
            x : event.clientX,
            y : event.clientY
    
        };
        div2.style.left = (mousePosition.x + offset[0]) + 'px';
        div2.style.top  = (mousePosition.y + offset[1]) + 'px';
    }
}, true);
