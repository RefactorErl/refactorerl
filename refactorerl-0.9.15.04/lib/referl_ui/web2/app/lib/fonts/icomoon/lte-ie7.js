/* Load this script using conditional IE comments if you need to support IE 7 and IE 6. */

window.onload = function() {
	function addIcon(el, entity) {
		var html = el.innerHTML;
		el.innerHTML = '<span style="font-family: \'icomoon\'">' + entity + '</span>' + html;
	}
	var icons = {
			'icon-play' : '&#xe000;',
			'icon-spinner' : '&#xe001;',
			'icon-drawer' : '&#xe002;',
			'icon-history' : '&#xe003;',
			'icon-enter' : '&#xe004;',
			'icon-arrow-left' : '&#xe005;',
			'icon-arrow-right' : '&#xe006;',
			'icon-wondering' : '&#xe007;',
			'icon-file-css' : '&#xe008;',
			'icon-archive' : '&#xe009;',
			'icon-ok' : '&#xf00c;',
			'icon-caret-right' : '&#xf0da;',
			'icon-caret-down' : '&#xf0d7;',
			'icon-circle-blank' : '&#xf10c;',
			'icon-circle' : '&#xf111;',
			'icon-dot' : '&#xe00a;',
			'icon-cancel' : '&#xe00b;'
		},
		els = document.getElementsByTagName('*'),
		i, attr, html, c, el;
	for (i = 0; ; i += 1) {
		el = els[i];
		if(!el) {
			break;
		}
		attr = el.getAttribute('data-icon');
		if (attr) {
			addIcon(el, attr);
		}
		c = el.className;
		c = c.match(/icon-[^\s'"]+/);
		if (c && icons[c[0]]) {
			addIcon(el, icons[c[0]]);
		}
	}
};