{% extends "admin_base.tpl" %}
{% block content %}

<h2>{_ Social Buffer _}</h2>
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
</ul>

<div id="content">
 {% include "buffer_list.tpl" %}
</div>

</div>

{% lib "js/buffer.js" %}
{% endblock %} 
