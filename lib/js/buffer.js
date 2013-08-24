

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


$(".edit-message-btn").live("click",function(e) {
  parent = $(this).parent().parent().parent().parent();
  id = parent.attr('data-buffer-id');
  // clear all the fields in the form
  $("#edit-buffer-form").clearForm();
  message = $("p#message-" + id).text();
  schedule = $("time#" + id).text();
  $("input#edit-buffer-id").val(id);
  $("textarea.message").val(message);
  $("input#edit-buffer-schedule" ).val(schedule);  
  $("#edit-buffer-modal").modal();
});


$("button#update-buffer-btn").live("click", function(e) {
    e.preventDefault();
    $("#edit-buffer-form").submit();
    z_growl_add("Updating buffer");
    $("#edit-buffer-modal").modal('hide');
});     

$("#edit-buffer-btn").live('click', function(e){
    e.preventDefault();
    parent = $(this).parent().parent().parent().parent();
    id = parent.attr('data-buffer-id');
    z_notify('delete-buffer', { 
	z_delegate: 'controller_admin_buffer', 
	buffer_id: id
    });
    parent.parent().hide();
});
           
