# crunghc: runghc replacement with automatic caching

## Usage

 crunghc \<script\>.hs \<args...\>

If this is the first time \<script\>.hs is run, crunghc compiles it to an
executable file and stores in a cache directory (~/.crunghc/ on Unix).
Next time, it will automatically use the cached version.
