# Mod_Buffer for Zotonic
===========================
Automagically share articles, pictures, videos, RSS Feed links through the day!
Inspired by http://bufferapp.com

## UI sections
Tentative user interface that will be rendered under the module's
settings dispatch(http://example.com/admin/buffer)
 
* Buffer 
  1. Post at a specific time every day(10am)
  2. Post on a specific date (Example: Jun-17-2013)
  3. Post between two specific times(10am-4pm)
  4. Categorize buffers. For example, add item1 and item2 to Party Buffer and apply the same schedule to them

* Schedule :
  1. view by categories
  2. view by successful and failed
  3. view all
  4. view only successful/failed ones
  5. view today's schedule(default), If none, view all
  6. view by date(D/M/Y)

* Settings
 1. Enable url shortening ?
 2. Log activities ?
 3. Rebuffer failed buffers?
 4. Add suport for timezones, so posts/updates are made relative to user's local time

**Actions for a buffer item : add, delete, share now, edit, change schedule**


## Contributing
1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added an awesome new feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

# Useful links:
1. http://blog.astekk.se/en/article/343/writing-your-own-zotonic-module-part-i
2. http://courses.cms.caltech.edu/cs11/material/erlang/
3. http://intertwingly.net/blog/2007/08/28/Parsing-Atom-with-Erlang
4. http://www.trapexit.org/How_to_write_an_RSS_aggregator
5. http://zotonic.com/docs
