

$('#buffer_menu a').click(function(e) {
    e.preventDefault();
    $(this).tab('show');
 });

$("#new_buffer_form").live('mouseover',function(e) {
  $('.datepicker').datepicker()
});

