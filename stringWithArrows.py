def stringWithArrows(line, posStart, posEnd):
    result = ''

    # Calculate indices
    idxStart = max(line.rfind('\n', 0, posStart.idx), 0)
    idxEnd = line.find('\n', idxStart + 1)
    if idxEnd < 0: idxEnd = len(line)
    
    # Generate each line
    lineCount = posEnd.ln - posStart.ln + 1
    for i in range(lineCount):
        # Calculate line columns
        linee = line[idxStart:idxEnd]
        colStart = posStart.col if i == 0 else 0
        colEnd = posEnd.col if i == lineCount - 1 else len(linee) - 1

        # Append to result
        result += linee + '\n'
        result += ' ' * colStart + '^' * (colEnd - colStart)

        # Re-calculate indices
        idxStart = idxEnd
        idxEnd = line.find('\n', idxStart + 1)
        if idxEnd < 0: idxEnd = len(line)

    return result.replace('\t', '')