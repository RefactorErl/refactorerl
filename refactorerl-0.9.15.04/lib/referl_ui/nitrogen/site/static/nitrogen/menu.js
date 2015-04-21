function activateMenu(sender){
	var menus = document.getElementsByClassName('menu_item');
	var id=document.getElementById(sender.id+'_img');
	for(var i=0;i<menus.length;i++){
		menus[i].src="/images/menu.png";
	}
	id.src="/images/menu_active.png";
}

function inactivateMenu(sender){
	var id=document.getElementById(sender.id+'_img');
	id.src="/images/menu.png";
}
