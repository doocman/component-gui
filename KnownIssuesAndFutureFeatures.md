
# Known Issues

## Design:

- Manual management of rerender when changing components. Should find a way to 'automate' it.
- Currently most of the framework uses pixels, but it should use 'points' the majority of the time.
- Must differentiate between pixel and point boxes, coordinates and sizes.

## Performance:

- Text must be cached more properly with a prerendered texture (when available).
