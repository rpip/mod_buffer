# Mod_Buffer for Zotonic
Automagically share articles, pictures, videos, RSS Feed links through the day!
Inspired by http://bufferapp.com, and currently supports only Twitter.
This module uses the mod_cron module for Zotonic, and the erlang-oauth library.
It comes with a simple UI for adding, editing, deleting, and sharing buffers.

## How it works
First, create a Twitter application and copy the application OAuth keys, and your acccess tokens.
These keys will be used to automatically post the feeds to your timeline.

Add the following configuration keys to your Zotonic application :

* mb_twitter_csec - Twitter Consumer secret
* mb_twitter_ckey - Twiter Consumer key
* mb_access_token_secret - Twitter Access Token Secret
* mb_access_token - Twitter Access Token


## Contributing
1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added an awesome new feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request 


## NOTE
Still in exeprimental stage, and not ready for use, yet.

