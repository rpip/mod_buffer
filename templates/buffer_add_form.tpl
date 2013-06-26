{% wire id="new_buffer_form" type="submit" postback="create_buffer" %}
<form id="new_buffer_form" method="post" action="postback" class="form-horizontal">
<div class="control-group">
    <label class="control-label" for="content">Content</label>
    <div class="controls">
      <textarea rows="3" name="content" placeholder="140 maximum characters"></textarea>
     </div>
</div>

<div class="control-group">
    <label class="control-label" for="tags">Tags</label>
    <div class="controls">
     <input type="text" name="tags" placeholder="tag1, tag2,tag3..."/>
    </div>
</div>

<div class="control-group">
    <label class="control-label" for="schedule">Schedule</label>
    <div class="controls">
<input type="text" value="02-16-2013" name="schedule" class="datepicker" format="dd-mm-yyyy">
</div>
</div>

<div class="control-group">
    <label class="control-label" for="destination">Destination</label>
    <div class="controls">
      <select name="destination">
      <option value="t">Twitter</option>
      <option value="fb">Facebook</option>
      <option value="g">Google Plus</option>
      <option value="e">Email</option>
      </select> 
    </div>
</div>

<div class="control-group">
    <label class="control-label" for="status">Published</label>
     <div class="controls">
      <input type="checkbox" name="status">
    </div>
</div>

<div class="control-group">
     <div class="controls">
      <button id="save_buffer_btn" class="btn-primary" type="submit">Submit</button>
    </div>
</div>

 
</form>
