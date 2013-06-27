{% extends "admin_base.tpl" %}
{% block content %}

<h2>{_ Social Buffer _}</h2>
    <div class="widget">

        <h3 class="widget-header">{_ Here you can manage all your buffers. _}</h3>
        <div class="widget-content">

<div id="admin_buffer">
 <ul class="nav nav-tabs" id="buffer_menu">
  <li class="active buffer-menu">
  {% wire id="list" postback={buffer_list} target="content" %}
    <a href="#" id="list">Buffer</a>
  </li>
  <li>
 {% wire id="logs" postback={buffer_logs} target="content" %}
   <a href="#" clas="buffer_menu" id="logs">Log</a>
 </li>
  <li>
 {% wire id="help" postback={buffer_help} target="content" %}
   <a href="#" clas="buffer_menu" id="help">Help</a>
 </li>
  <li>
 {% wire id="buffer_new_form" postback={buffer_new_form} target="content" %}
   <a href="#" class="buffer_menu" id="buffer_new_form">Add New Buffer</a>
 </li>
</ul>

<div id="content">
 {% include "buffer_list.tpl" %}
</div>

</div>
</div>
</div>

{% lib "css/bootstrap-timepicker.css" %}
{% lib "css/buffer.css" %}
{% lib "js/bootstrap-timepicker.js" %}
{% lib "js/bootstrap-maxlength.min.js" %}
{% lib "js/buffer.js" %}
{% endblock %} 
