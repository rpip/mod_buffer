

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

$("p.message").live('click',(function(e) {
  $(this).toggle();
  id = $(this).attr('id');
  textarea = "#msg-textarea-" + id;
  $(textarea).toggle();
}));


//$(".tooltip").hover(function(e) {$(this).tooltip('toggle')});
//$(".tooltip").tooltip('show');



