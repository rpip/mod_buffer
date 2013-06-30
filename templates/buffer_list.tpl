<div class="timeline">
<ol id="buffer_list" class="timeline">

{% for buffer in m.buffer  %}
  <li id="buffer-{{ buffer.id }}" >
    <div data-buffer-id="{{ buffer.id }}" id="buffer_content" class="content">
      <p id="message-{{ buffer.id }} " class="message" style="display:block;">{{ buffer.message }}</p>


<textarea id="msg-textarea-{{ buffer.id }}" class="message" style="display:none;">{{ buffer.message }}</textarea>

      <div class="meta clearfix">
        <p class="details pull-left">
          <span>
            <time original-title="Scheduled by Social Buffer"> 
 <a data-toggle="tooltip" title="Schedule : {{ buffer.schedule }}">
i class="icon-time"></i> {{ buffer.schedule }}</a>
            </time>
          </span>
          </p>
        
        <ul class="actions pull-right"  id={{ #buffer.id }}>
          <li>
          <a class="edit-message-btn" ><i class="icon-edit"></i>Edit</a></li>
          

          <li>
           <a><i class="icon-trash"></i>
            {% button text="Delete"  class="buffer-action-btn" postback={buffer_delete id=buffer.id target_id="content"} %}

           </a> 
          </li>


          <li>
           <a><i class="icon-share"></i>
            {% button text="Share  Now"  class="buffer-action-btn" postback={buffer_share id=buffer.id} %}
           </a> 
          </li>


      </ul>


      </div>
    </div>
  </li>
  {% endfor %}
</ol>
</div>
