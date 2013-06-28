

$('#buffer_menu a').click(function(e) {
    e.preventDefault();
    $(this).tab('show');
 });



$("#new_buffer_form").live('mouseover',function(e) {
  $('#datepicker1').datepicker();
  $('#timepicker1').timepicker({defaultTime:false});
  $('.message').maxlength({
    alwaysShow: true,
    threshold: 10,
    warningClass: "label label-info",
    limitReachedClass: "label label-warning",
    placement: 'top',
    preText: 'used ',
    separator: ' of ',
    postText: ' chars.'
  });
});


$("p.message").live('hover',(function(e) {
  $(this).toggleClass("message_editmode");
}));

$("#buffer_list p.message").live('click',(function(e) {
  $(this).toggle();
  id = $(this).parent().attr('data-buffer-id');
  textarea = "#msg-textarea-" + id;
  $(textarea).toggle();
}));

$("#buffer_list textarea.message").live('mouseleave',function(e) {
  parent = $(this).parent();
  id = parent.attr('data-buffer-id');
  message = "#message-" + id;
  parent.children(':hidden').show();
  $(this).toggle();
});






