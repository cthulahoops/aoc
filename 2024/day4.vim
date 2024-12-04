function! SearchMatches(search_str)
    let l:matches = []
    let l:count = 0

    let l:directions = [[1,0], [1,1], [0,1], [-1,1], [-1,0], [-1,-1], [0,-1], [1,-1]]

    for l:dy_dx in l:directions
        let l:dy = l:dy_dx[0]
        let l:dx = l:dy_dx[1]

        let l:results = SearchMatchesDirection(a:search_str, l:dy, l:dx)
        let l:count += l:results.count

        for l:match in l:results.matches
            call add(l:matches, l:match)
        endfor
    endfor

    return {
        \ 'count': l:count,
        \ 'matches': l:matches,
        \ 'pattern': a:search_str
        \ }
endfunction

function! SearchMatchesDirection(search_str, dy, dx)
    let l:saved_pos = getpos('.')
    let l:count = 0
    let l:pattern_length = strlen(a:search_str)
    let l:last_line = line('$')
    let l:max_col = 0
    let l:matches = []

    for l:lnum in range(1, l:last_line)
        let l:line_length = strlen(getline(l:lnum))
        let l:max_col = max([l:max_col, l:line_length])
    endfor

    for l:col in range(1, l:max_col)

        for l:start_line in range(1, l:last_line + 1)
            let l:match = 1
            let l:matched_pos = []

            for l:offset in range(l:pattern_length)
                let l:y = l:start_line + l:offset * a:dy
                let l:x = l:col + l:offset * a:dx

                let l:line_text = getline(l:y)

                let l:ch = l:line_text[l:col - 1]

                if l:line_text[l:x - 1] !=# a:search_str[l:offset]
                    let l:match = 0
                    break
                endif

                call add(l:matched_pos, [l:y, l:x])
            endfor

            if l:match
                let l:count += 1
                call add(l:matches, l:matched_pos)
            endif
        endfor
    endfor

    return {
        \ 'count': l:count,
        \ 'matches': l:matches,
        \ 'pattern': a:search_str
        \ }
endfunction

function! SearchXMas()
    let l:saved_pos = getpos('.')
    let l:count = 0
    let l:last_line = line('$')
    let l:max_col = 0
    let l:matches = []

    for l:lnum in range(1, l:last_line)
        let l:line_length = strlen(getline(l:lnum))
        let l:max_col = max([l:max_col, l:line_length])
    endfor

    for l:col in range(1, l:max_col)

        for l:start_line in range(1, l:last_line + 1)

            for l:target in ["AMSMS", "ASMSM", "AMSSM", "ASMMS"]
                let l:match = 1
                let l:matched_pos = []

                for l:offset in [[0,0,0], [-1,1,1], [1,-1,2], [-1,-1,3], [1,1,4]]
                    let l:y = l:start_line + l:offset[0] * 1
                    let l:x = l:col + l:offset[1] * -1
                    let l:search_pos = l:offset[2]

                    let l:line_text = getline(l:y)

                    let l:ch = l:line_text[l:col - 1]

                    if l:line_text[l:x - 1] !=# l:target[l:search_pos]
                        let l:match = 0
                        break
                    endif

                    call add(l:matched_pos, [l:y, l:x])
                endfor

                if l:match
                    let l:count += 1
                    call add(l:matches, l:matched_pos)
                endif
            endfor

        endfor
    endfor

    return {
        \ 'count': l:count,
        \ 'matches': l:matches,
        \ 'pattern': "X-MAS"
        \ }
endfunction

function! HighlightMatches(results)
    syntax clear SearchMatch

    for l:match_set in a:results.matches
        for l:pos in l:match_set
            let l:line = l:pos[0]
            let l:col = l:pos[1]
            execute 'syntax match SearchMatch /\%' . l:line . 'l\%' . l:col . 'c./'
        endfor
    endfor

    highlight SearchMatch ctermbg=green guibg=green
    echo 'Found ' . a:results.count . ' matches for ' . a:results.pattern
endfunction

command! -nargs=1 HLMatches call HighlightMatches(SearchMatches(<q-args>))
command! HLMatches2 call HighlightMatches(SearchXMas())

map <leader>cm :so day4.vim<CR>:HLMatches<Space>XMAS<CR>
map <leader>cx :so day4.vim<CR>:HLMatches2<CR>
