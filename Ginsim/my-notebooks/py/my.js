function zoom(mutant) {
	var selected_children = document.getElementById(mutant.id).children;
	//console.log(selected_children.length);
	var zoom_box = document.getElementById("zoom_box");
	var zoom_box_children = document.getElementById("zoom_box").children;

	while(zoom_box_children.length) { // remove children of zoom_box
		zoom_box.removeChild(zoom_box_children[0]);
	}

	for(i=0; i < selected_children.length; i++) { // loop over selected children (stable states)
        //console.log(selected_children[i])
        var cloned_child = selected_children[i].cloneNode(true); // copy this stable state
        console.log(cloned_child.children.length);
        if ((cloned_child.children.length > 0) && (cloned_child.children[0].className=='text')) {
            //console.log(cloned_child.children.length);
            //console.log(cloned_child.children);
            cloned_child.children[0].style.display = 'block'; // display text
        }
        //console.log(cloned_child);
        //console.log(cloned_child.innerHTML);
    	zoom_box.appendChild(cloned_child);
	}

var el = document.getElementById('zoom_box_back');
el.scrollIntoView(true);

}


