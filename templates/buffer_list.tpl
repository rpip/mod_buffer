<ul id="buffer_list">
{% for id in [1,2,3,4,5,6] %}
   <li>
   <span>Buffer Number-{{#id}}</span>
   <span id="buffer-{{#id}}">edit | delete | share now</span>
   </li>
{% endfor %}
</ul>
