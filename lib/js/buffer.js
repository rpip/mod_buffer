

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

$("#buffer_list textarea.message").live('mouseout mouseleave focusout blur',function(e) {
  parent = $(this).parent();
  id = parent.attr('data-buffer-id');
  parent.children(':hidden').show();
  $(this).toggle();
});

$(".edit-message-btn").live("click",function(e) {
  parent = $(this).parent().parent().parent().parent();
  id = parent.attr('data-buffer-id');
  textarea = "#msg-textarea-" + id;
  parent.children("p.message").toggle();
  $(textarea).toggle();

});


$("textarea.message").live("change", function(e) {
    message = "p#message-7" + $(this).parent().attr('data-buffer-id');
    console.log(message);
    $(message).data($(this).data());
    Postback = {"message":$(this).data()};
    z_do_postback($(this).attr('id'),Postback,{});
});     




           
