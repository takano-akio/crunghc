# crunghc: runghc replacement with automatic caching

## Usage

 crunghc \<script\>.hs \<args...\>

When a script is first run with crunghc, it is compiled to an executable
file and stored in a cache directory (~/.crunghc/ on Unix).
When it is run again, crunghc reuses the cached executable if no change have
been made to the script since it was last compiled.
