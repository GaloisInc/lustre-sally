function moduleViewTrace () {

  // The state
  var curStep = 0
  var visCS = {}
  var maxStep = 0

  // Names
  function stepName(i) { return 'step-' + i }
  var stepDisplayId = 'step-display'
  function lineName(i) { return 'line-' + i }


  // Show steps
  function showStep(s) {
    if (s > maxStep) return
    if (s < 0) return

    $('.' + stepName(curStep)).removeClass('vis-step')
    $('.' + stepName(s)).addClass('vis-step')
    $('#' + stepDisplayId).text(s)
    curStep = s
    displayCurStep()
  }

  function nextStep() { showStep(curStep+1) }
  function prevStep() { showStep(curStep-1) }

  function displayCurStep() {
    $('#' + stepDisplayId).text(curStep === 0 ? 'initializing' : curStep)
  }

  function stepButtons() {
    var prev = $('<button/>')
               .addClass('step-button')
               .text('<<')
               .click(prevStep)
    var next = $('<button/>')
               .addClass('step-button')
               .text('>>')
               .click(nextStep)
    var label = $('<span/>').text('step:')
    var display = $('<span/>').attr('id',stepDisplayId)
    return [prev,label,display,next]
  }

  // Toggle call sites
  function toggleCallsite(cid) {
    var cs = $('.call-site.' + cid)
    var things = $('.value-box.' + cid)
    if (visCS[cid]) {
        things.removeClass('vis-cs')
        cs.removeClass('active')
        visCS[cid] = false
    }
    else {
      things.addClass('vis-cs')
      cs.addClass('active')
      visCS[cid] = true
    }
  }

  // Hilighting things
  function markLines(a,b) {
    for (var i = a; i <= b; ++i) {
      $('#' + lineName(i)).addClass('marked')
    }
  }

  function unmarkLines() {
    $('.line').removeClass('marked')
  }

  function markCallsite(cid) {
    $('.call-site.' + cid).addClass('marked')
  }

  function unmarkCallsite() {
    $('.call-site').removeClass('marked')
  }


  // Drawing

  function drawSource(source, attrs) {
    // source : String
    // attrs : { Line ~> [ {from,to: Int, attr: [Attr]} ] }
    // Line = String
    // Attr = { step: Int, cid: String, value: String }

    var dom = $('<div/>').addClass('code')

    function getAnn(ln) {
      var res = attrs[ln.toString()]
      return (res === undefined) ? [] : res
    }

    // Render the lines
    var lines = source.split('\n')
    jQuery.each(lines,function(lineNoPrev,txt) {
      var lineNo = lineNoPrev + 1   // lines start at 1
      var ln = $('<div/>').addClass('line').attr('id',lineName(lineNo))
      ln.append($('<div/>').addClass('line-no').text(lineNo))
      var start = 0
      jQuery.each(getAnn(lineNo),function(ix,ann) {
        var from = ann.from - 1   // columns start at 1
        if (from > start) {
          ln.append($('<span/>').text(txt.substring(start,from)))
        }
        start = ann.to
        var annDom = $('<span/>')
                     .addClass('annot')
                     .text(txt.substring(from,start))
        ln.append(annDom)

        jQuery.each(ann.attr,function(ix,attr) {
          if (attr.value === undefined) {
            annDom.addClass('call-site')
                  .addClass(attr.cid)
            var src = attr.source
            annDom.hover( function() { markLines(src.from,src.to)}
                        , unmarkLines
                        )
                  .click( function() { toggleCallsite(attr.cid) })
          } else {
             var val = $('<div/>')
                     .addClass('value-box')
                     .addClass(attr.cid)
                     .addClass(stepName(attr.step))
                     .append($('<span/>').text('=')
                            ,$('<span/>')
                             .addClass('value')
                             .text(attr.value))
             if (attr.cid === 'cs_top') val.addClass('vis-cs')
             val.hover( function() { markCallsite(attr.cid) }
                      , unmarkCallsite )
             if (attr.step > maxStep) maxStep = attr.step
             ln.append(val)
          }
        })

      })
      ln.append($('<span/>').text(txt.substring(start)))

      dom.append(ln)
    })

    return dom
  }

  // The public interface
  return { drawSource: drawSource
         , stepButtons: stepButtons
         , showStep: showStep
         }

}
