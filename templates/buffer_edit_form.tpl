<!-- Modal -->
<div id="edit-buffer-modal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-header">
    <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
    <h3 id="myModalLabel">Edit Buffer</h3>
  </div>
  <div class="modal-body">
{% wire id="edit-buffer-form" type="submit" postback="update_buffer" %}
<form id="edit-buffer-form" method="post" action="postback" class="form-horizontal">

<div class="control-group">
    <label class="control-label" for="message">Message</label>
    <div class="controls">
      <textarea rows="3" name="message" id="edit-buffer-message" placeholder="140 maximum characters" 
      class="message" maxlength="146"></textarea>
     </div>
</div>


<div class="control-group">
    <label class="control-label" for="schedule">Schedule</label>
    <div class="controls">
            <input  type="text" id="edit-buffer-schedule" name="schedule"  class="input" placeholder="{weekly, wed, {2, am}}">
    </div>
</div>


<div class="control-group">
    <label class="control-label" for="destination">Destination</label>
    <div class="controls">
      <input type="checkbox" value="t" name="destination" /> Twitter
      <input type="checkbox" value="fb" name="destination" /> Facebook
    </div>
</div>

<div class="control-group">
    <label class="control-label" for="status">Published</label>
     <div class="controls">
      <input type="checkbox" name="status">
    </div>
</div>

<input type="hidden" name="id" id="edit-buffer-id"/>

</form>
  </div>
  <div class="modal-footer">
    <button class="btn" data-dismiss="modal" aria-hidden="true">Close</button>
    <button id="update-buffer-btn" class="btn btn-primary">Save changes</button>
  </div>
</div>

