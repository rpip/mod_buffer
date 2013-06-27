<div>
<p
when()       = {daily, period()}
             | {weekly, dow(), period()} 
             | {monthly, dom(), period()}
dow()        = mon | tue | wed | thu | fri | sat | sun
dom()        = integer()
period()     = time() | [time()] | {every, duration(), constraint()}
duration()   = {integer(), hr | min | sec}
constraint() = {between, time(), time()}
time()       = {integer(), am | pm} | {integer(), integer(), am | pm}
</p>

<p>
{daily, {every,{5,sec},{between,{0,am},{11,59,pm}}}},
{weekly, wed, {2, am}},
</p>
</div>
