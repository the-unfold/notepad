## Plan:
### Step 2: process events in background

- sql table for notepad entries - notes: serial unique note_id, user_id foreign key, note_content, index по user_id
- добавить и обрабатывать событие добавления note
- ping метнись кабанчиком
- фронт всегда поллит сам, если вообще заинтересован в результатах обработки. Возможно сочетание советов и поллинга.

Подходы:
A. Фронт всегда получает id события и сам поллит результат, когда заинтересован в нём.
### Step 3: read results (projected state)

- Optional (for separate projections): Get n events starting from m
### Future ideas

- return id and ts
- уведомлять программу-обработчик событий о наступлении события, но не кидать само событие - пусть обработчик события сам лезет в базу и достаёт пачку событий. Если такое уведомление проебётся, то обработчик по таймеру сам осуществит попытку чтения.