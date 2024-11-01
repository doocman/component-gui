# Event Handling

## Raw or interpreted

In general, it is the raw events (coming directly from the backend) that is propagated through all the various
components. But Component GUI incorporates facilities to 'interpret' these events into more high-level, intention-based
events (raw mouse button up/down to preliminary_primary_click or context_menu_click and so on).
