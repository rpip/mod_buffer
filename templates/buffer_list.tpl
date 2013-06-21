<div class="timeline">
<ol id="buffer_list" class="timeline">

{% for id in [1,2,3,4,5,6] %}
  <li id="buffer-{{#id}}">
    <div id="buffer_content" class="content">

      <p class="message" style="display: block;">test</p>
       <!-- <textarea class="message" style="display: --
          none;">test</textarea> -->
    
      <div class="meta clearfix">
        <p class="details pull-left">
          <span>
            <time original-title="Scheduled by Social Buffer"> 
              <i class="icon-time"></i> 4:30 pm
            </time>
          </span>
          </p>
        
        <ul class="actions pull-righ">
          <li class="edit"><a><i class="icon-edit"></i>Edit</a></li>
          <li class="delete"><a><i class="icon-trash"></i>Delete</a></li>
          <li class="share-now"><a><i class="icon-share"></i>Share Now</a></li>
        </ul>

      </div>
    </div>
  </li>
  {% endfor %}
</ol>
</div>
