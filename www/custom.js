closeBox = function(boxid) {
  var box = $('#' + boxid).closest('.box');
  if (!box.hasClass('collapsed-box')) {
    box.find('[data-widget=collapse]').click();
  }
};

openBox = function(boxid) {
  var box = $('#' + boxid).closest('.box');
  if (box.hasClass('collapsed-box')) {
    box.find('[data-widget=collapse]').click();
  }
};
