<div class="timeline">
<ol id="buffer_list" class="timeline">

{% for buffer in m.buffer  %}
  <li id="buffer-{{ buffer.id }}">
    <div data-buffer-id="{{ buffer.id }}"id="buffer_content" class="content">
      <p id="message-{{ buffer.id }} " class="message" style="display:block;">{{ buffer.message }}</p>
       <textarea id="msg-textarea-{{ buffer.id }}" class="message" style="display:none;">{{ buffer.message }}</textarea>
    
      <div class="meta clearfix">
        <p class="details pull-left">
          <span>
            <time original-title="Scheduled by Social Buffer"> 
 <a data-toggle="tooltip" title="Schedule : {{ buffer.schedule }}">              <i class="icon-time"></i> {{ buffer.schedule }}</a>
            </time>
          </span>
          </p>
        
        <ul class="actions pull-righ">
          <li class="edit" id="{{ buffer.id }}">
          <a class="edit-message-btn"  href="#edit/{{ buffer.id }}"><i class="icon-edit"></i>Edit</a></li>
          
          <li class="delete">
           {% wire id="delete-{{ buffer.id }}"  action={growl text="buffer deleted"}  %}
           <a id="delete-{{ buffer.id }}" href="#delete-{{ buffer.id }}"><i class="icon-trash"></i>Delete</a>
          </li>

          <li class="share-now">
           {% wire id="share-{{ buffer.id }}"  action={growl text="buffer shared"}  %}
           <a id="share-{{ buffer.id }}" href="#share-{{ buffer.id }}"><i class="icon-share"></i>Share Now</a></li>

      </ul>


      </div>
    </div>
  </li>
  {% endfor %}
</ol>
</div>
