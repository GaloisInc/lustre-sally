function annotationName(line,from,to) {
  return 'ann_' + line + '_' + from + '_' + to
}



function drawSource(source, trace) {
  // source      : String
  // annotations : [Annotation]
  // Annotation  = { line, from, to : Int, value: Showable }
  // Assumption: annotations do not overlap

  var dom = $('<div/>').addClass('code')

  var annMap = {}
  function getAnn(line) {
    var thisLine = annMap[line]
    return thisLine === undefined? [] : thisLine
  }

  // Group annotations by line
  jQuery.each(trace, function(step,tr) {
    jQuery.each(tr, function(ix,ann) {
      var thisLine = getAnn(ann.line)
      var newAnn = jQuery.extend({step:step},ann)
      thisLine.push(newAnn)
      annMap[ann.line] = thisLine
    })
  })

  // Render the lines
  var lines = source.split('\n')
  jQuery.each(lines,function(lineNoPrev,txt) {
    var lineNo = lineNoPrev + 1
    var ln = $('<div/>').addClass('line')
    var sorted = getAnn(lineNo).sort(function(a,b) { return a.from - b.from })
    var start = 0
    jQuery.each(sorted,function(ix,ann) {
      var from = ann.from - 1   // columns start at 1
      if (from > start ) {
        ln.append($('<span/>').text(txt.substring(start,from)))
      }
      start = ann.to
      var annDom = $('<span/>')
                   .addClass('annot')
                   .text(txt.substring(from,start))

      var valAnn = ann.value
      if (valAnn === undefined) {
        // XXX
      } else {
        var val = $('<span/>')
                  .addClass('value')
                  .addClass(ann.cid)
                  .addClass('step-' + ann.step)
                  .text(ann.value)
        ln.append(annDom,val)
      }
    })
    ln.append($('<span/>').text(txt.substring(start)))

    dom.append(ln)
  })

  return dom
}

