

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





