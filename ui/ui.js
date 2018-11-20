function moduleViewTrace () {

  // The state
  var curStep = 0
  var callStack = [ "cs_top" ]
  var maxStep = 0

  // Names
  function stepName(i) { return 'step-' + i }
  var stepDisplayId = 'step-display'


  // Show steps
  function showStep(s) {
    if (s > maxStep) return
    if (s < 0) return

    $('.' + stepName(curStep)).hide()
    $('.' + stepName(s)).show()
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
      var ln = $('<div/>').addClass('line')
      var start = 0
      jQuery.each(getAnn(lineNoPrev+1),function(ix,ann) {
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
          } else {
             var val = $('<span/>')
                     .addClass('value')
                     .addClass(attr.cid)
                     .addClass(stepName(attr.step))
                     .text(attr.value)
                     .hide()
             if (attr.step > maxStep) maxStep = attr.step
             var eq = $('<span/>')
                    .text('=')
                    .addClass(attr.cid)
                    .addClass(stepName(attr.step))
                    .hide()
             ln.append(eq,val)
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
