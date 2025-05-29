var mousePosition;
var offset = [0,0];
var div;
var isDown = false;

table = document.getElementById("dynamicland");
tableWidth = 500;
tableHeight = 300;
pageWidth = 100;
pageHeight = 100;

contains = function(element, x, y) {
	rect = element.getBoundingClientRect();
	return x > rect.x && (x + pageWidth) < (rect.x + tableWidth) && y > rect.y && (y + pageHeight) < (rect.y + tableHeight) ;
}

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
	x = mousePosition.x + offset[0];
	y = mousePosition.y + offset[1];
        div.style.left = x + 'px';
        div.style.top  = y + 'px';
	if ( contains(table, x, y) ) {
		div.style.background = "red";
	} else {
		div.style.background = "white";
	}
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
	x = mousePosition.x + offset[0];
	y = mousePosition.y + offset[1];
        div2.style.left = x + 'px';
        div2.style.top  = y + 'px';
	if ( contains(table, x, y) ) {
		div2.style.background = "blue";
	} else {
		div2.style.background = "white";
	}
    }
}, true);
