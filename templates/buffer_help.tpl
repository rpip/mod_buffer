<div>
<p>
mod_buffer(Social Buffer) uses mod_cron for scheduling, and therefore adopts mod_cron's 
scheduling rules for when to execute buffer item. <br >Below is a quick overview of how to contruct 
a schedule, and some examples.

<pre>
when()       = {daily, period()}
             | {weekly, dow(), period()} 
             | {monthly, dom(), period()}
dow()        = mon | tue | wed | thu | fri | sat | sun
dom()        = integer()
period()     = time() | [time()] | {every, duration(), constraint()}
duration()   = {integer(), hr | min | sec}
constraint() = {between, time(), time()}
time()       = {integer(), am | pm} | {integer(), integer(), am | pm}
</pre>
</p>

<h2>Examples</h2>
<pre>

{daily, {every,{5,sec},{between,{0,am},{11,59,pm}}}},
{weekly, wed, {2, am}},
{daily, {10,30, am}}
</pre>
</div>
