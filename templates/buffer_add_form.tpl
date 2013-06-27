{% wire id="new_buffer_form" type="submit" postback="create_buffer" %}
<form id="new_buffer_form" method="post" action="postback" class="form-horizontal">
<div class="control-group">
    <label class="control-label" for="message">Message</label>
    <div class="controls">
      <textarea rows="3" name="message" placeholder="140 maximum characters" 
      class="message" maxlength="146"></textarea>
     </div>
</div>


<div class="control-group">
    <label class="control-label" for="day">Day</label>
    <div class="controls">
    <div class="input-append">
            <input id="datepicker1" type="text" name="day" class="input-small">
            <span class="add-on"><i class="icon-calendar"></i></span>
        </div>
</div>
</div>

<div class="control-group">
    <label class="control-label" for="time">Time</label>
    <div class="controls">
    <div class="input-append bootstrap-timepicker">
            <input id="timepicker1" type="text" name="time"  class="input-small">
            <span class="add-on"><i class="icon-time"></i></span>
        </div>
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


