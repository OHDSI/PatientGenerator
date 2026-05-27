// !preview r2d3 data = structure(list(event_id = c(1, 1), concept_id = c(NA, 44191562), person_id = c(1, 1), start_date = c("2010-05-05", "2013-05-05"), end_date = c("2018-10-08", "2016-10-08"), type = c("observation_period","drug_exposure"), categories = 1:2), row.names = c(NA, -2L), class = "data.frame"),
/*console.log(JSON.stringify(data, null, 2));
console.table(data)*/

const margin = {
  right: 15,
  left: 15,
  top: 20,
  bottom: 20
};

const barHeight = 32
const gap  = 12
const axisPad = 12
const labelPad = 8
const measurementMarkerWidth = barHeight

function formatDate(value) {
  if (value === null || value === undefined || value === "") {
    return "NA";
  }

  return value;
}

function formatConceptId(value) {
  if (value === null || value === undefined || value === "") {
    return "NA";
  }

  return value;
}

function barLabel(d) {
  return `${d.type} | ${formatConceptId(d.concept_id)}`;
}

function tooltipText(d) {
  return [
    `Table: ${d.type}`,
    `Concept ID: ${formatConceptId(d.concept_id)}`,
    `Person ID: ${d.person_id}`,
    `Event ID: ${d.event_id}`,
    `Start: ${formatDate(d.start_date)}`,
    `End: ${formatDate(d.end_date)}`
  ].join("\n");
}

function labelX(scale, d) {
  if (d.type === "measurement") {
    return scale(new Date(d.start_date)) - measurementMarkerWidth - labelPad;
  }

  return scale(new Date(d.start_date)) - labelPad;
}

function trianglePoints(scale, d, yScale) {
  const x = scale(new Date(d.start_date));
  const yTop = yScale(d.categories);
  const yBottom = yTop + barHeight;
  const xLeft = x - (measurementMarkerWidth / 2);
  const xRight = x + (measurementMarkerWidth / 2);

  return [
    `${x},${yTop}`,
    `${xLeft},${yBottom}`,
    `${xRight},${yBottom}`
  ].join(" ");
}

// Colour functions
// Colour functions
function startColor(d, i) {
  if (d.type == "observation_period") {
    return "#E1B12C";
  } else if (d.type == "drug_exposure") {
    return "#00B894";
  } else if (d.type == "condition_occurrence") {
    return "#D81B60";
  } else if (d.type == "measurement") {
    return "#E53935";
  } else if (d.type == "procedure_occurrence") {
    return "#1E88E5";
  }
  return "#999";
}

function dragColor(type) {
  if (type == "observation_period") {
    return "#FFD54F";
  } else if (type == "drug_exposure") {
    return "#55E6C1";
  } else if (type == "condition_occurrence") {
    return "#F06292";
  } else if (type == "measurement") {
    return "#FF6F60";
  } else if (type == "procedure_occurrence") {
    return "#64B5F6";
  }
  return "#999";
}

function endColor(type) {
  if (type == "observation_period") {
    return "#E1B12C";
  } else if (type == "drug_exposure") {
    return "#00B894";
  } else if (type == "condition_occurrence") {
    return "#D81B60";
  } else if (type == "measurement") {
    return "#E53935";
  } else if (type == "procedure_occurrence") {
    return "#1E88E5";
  }
  return "#999";
}

let currentTransform = d3.zoomIdentity;

var domainStart = new Date(1980, 0, 1);
    domainEnd = new Date(2026, 0, 1);

r2d3.onRender(function(data, svg, width, height, options) {
  const tooltipId = "patient-generator-timeline-tooltip";
  let tooltip = d3.select(`body #${tooltipId}`);

  if (tooltip.empty()) {
    tooltip = d3.select("body")
      .append("div")
      .attr("id", tooltipId)
      .style("position", "fixed")
      .style("z-index", "9999")
      .style("pointer-events", "none")
      .style("display", "none")
      .style("padding", "8px 10px")
      .style("border-radius", "6px")
      .style("background", "rgba(17, 17, 17, 0.92)")
      .style("color", "#fff")
      .style("font-family", "sans-serif")
      .style("font-size", "12px")
      .style("line-height", "1.4")
      .style("white-space", "pre-line")
      .style("box-shadow", "0 6px 18px rgba(0,0,0,0.22)");
  }

  function showTooltip(event, d) {
    tooltip
      .style("display", "block")
      .text(tooltipText(d));
    moveTooltip(event);
  }

  function showHoverLineAt(x) {
    const svgX = x + margin.left;

    hoverLine
      .style("display", "block")
      .raise()
      .attr("x1", svgX)
      .attr("x2", svgX);
  }

  function moveTooltip(event) {
    tooltip
      .style("left", `${event.clientX + 14}px`)
      .style("top", `${event.clientY + 14}px`);
  }

  function hideTooltip() {
    hoverLine.style("display", "none");
    tooltip.style("display", "none");
  }

  // Remove previous elements to prevent duplicates
  svg.selectAll("*").remove();

  const xScale = d3.scaleTime()
    .domain([domainStart, domainEnd])
    .range([0, width - margin.left - margin.right]);

  const yScale = d3.scaleBand()
      .domain(data.map(d => d.categories))
      .range([0, data.length * (barHeight + gap)])
      .paddingInner(gap / (barHeight + gap))
      .padding(0.2)

  const axisTop = d3.axisTop(xScale)
  const axisBottom = d3.axisBottom(xScale);
  const plotTop = margin.top;
  const plotBottom = height - margin.bottom;

  const hoverLine = svg
    .append("line")
    .attr("x1", 0)
    .attr("x2", 0)
    .attr("y1", plotTop)
    .attr("y2", plotBottom)
    .style("stroke", "rgba(17, 17, 17, 0.45)")
    .style("stroke-width", "1px")
    .style("stroke-dasharray", "4,4")
    .style("pointer-events", "none")
    .style("display", "none");

  var background = svg.attr("class", "hidden rectangle")
    .append("rect")
    .attr("class", "background")
    .attr("x", 0)
    .attr("y", margin.bottom)
    .attr("width", width)
    .attr("height", height - margin.bottom - margin.bottom)
    .style("fill", "white")

  var xAxisTop = svg
    .append("g")
    .attr("class", "axis_top")
    .attr("transform", (d,i)=>`translate(${margin.left} ${margin.top})`)
    .call(axisTop)

  var xAxisBottom = svg
    .append("g")
    .attr("class", "axis_bottom")
    .attr("transform", (d,i)=>`translate(${margin.left} ${height-margin.bottom})`)
    .call(axisBottom)

  function zooming(event) {
    
    currentTransform = event.transform;
    const xScaleUpdated = event.transform.rescaleX(xScale)

    xAxisTop
      .call(axisTop.scale(xScaleUpdated))

    xAxisBottom
      .call(axisBottom.scale(xScaleUpdated))

    g.selectAll("rect.rectGroup")
      .attr("x", d => xScaleUpdated(new Date(d.start_date)))
      .attr("width", d => xScaleUpdated(new Date(d.end_date)) - xScaleUpdated(new Date(d.start_date)))

    g.selectAll("polygon.measurementMarker")
      .attr("points", d => trianglePoints(xScaleUpdated, d, yScale))

    g.selectAll("circle.circleLeftGroup")
      .attr('cx', d => xScaleUpdated(new Date(d.start_date)))

    g.selectAll("circle.circleRightGroup")
      .attr('cx', d => xScaleUpdated(new Date(d.end_date)))

    g.selectAll("text.barLabel")
      .attr("x", d => labelX(xScaleUpdated, d))

  }

  function dragStart(event, d) {
    hideTooltip();
    // console.log(event)
    bar = d3.select(this)
    bar.style("fill", dragColor(d.type))
    Shiny.setInputValue(
    "bar_start",
     d,
     {priority: "event"}
     );
  }

  // Standard move
  function dragMove(event, d) {
    // console.log(event)
    let bar = d3.select(this)
    const width = +bar.attr("width")

    // Update from zoom event
    const zoom_state = d3.zoomTransform(svg.node());
    // Create new scale
    const xScaleUpdated = zoom_state.rescaleX(xScale);

    const end_date_position = event.x + width
    bar.attr("x", event.x)
    // Circle class selection
    var row = d3.select(this).attr("class").split(/\s+/).filter(s=>s.startsWith("row_"))[0];
    const widget = document.querySelector('.html-widget'); // or '#htmlwidget-xxxx'
    const root = widget.shadowRoot;                        // <- key
    // Left
    const circleLeftSelection = ".circleLeftGroup." + row
    d3.select(root).select(circleLeftSelection).attr('cx', event.x);
    // Right
    const circleRightSelection = ".circleRightGroup." + row
    d3.select(root).select(circleRightSelection).attr('cx', end_date_position);
    hoverLine.style("display", "none");
    // Update data
    d.start_date = xScaleUpdated.invert(event.x)
    d.end_date = xScaleUpdated.invert(end_date_position)

    let name_start_date, name_end_date;

    if (d.type == "condition_occurrence") {
      // Branch A: Special naming for conditions
      // We make sure to assign to the same variable names used later
      name_start_date = "#condition_occurrence-condition_start_date input";
      name_end_date = "#condition_occurrence-condition_end_date input";
      
    } else if (d.type == "procedure_occurrence") {
      
      name_start_date = "#procedure_occurrence-procedure_date input";
      name_end_date = "#procedure_occurrence-procedure_end_date input";
    
    } else if (d.type == "measurement") {
            
      name_start_date = "#measurement-measurement_date input";

    } else {
        // Branch B: Standard naming pattern
        name_start_date = "#" + d.type + "-" + d.type + "_start_date input";
        name_end_date = "#" + d.type + "-" + d.type + "_end_date input";
    }

    // 3. Format the dates
    const value_start_date = d3.timeFormat("%Y-%m-%d")(new Date(d.start_date));
    const value_end_date = d3.timeFormat("%Y-%m-%d")(new Date(d.end_date));

    // 4. Update the DOM
    d3.select(name_start_date)
      .property("value", value_start_date)
      .dispatch("change");

    d3.select(name_end_date)
      .property("value", value_end_date)
      .dispatch("change");

    d3.select(root).select(".barLabel." + row)
      .attr("x", event.x - labelPad)
      .text(barLabel(d));

/*    d3.select("#observation_period-observation_period_start_date input").property("value", value_start_date).dispatch("change");
    d3.select("#observation_period-observation_period_end_date input").property("value", value_end_date).dispatch("change");*/


    Shiny.setInputValue(
      "bar_move",
       d,
       {priority: "event"}
       );
  }

  function dragMeasurement(event, d) {
    let marker = d3.select(this)

    const zoom_state = d3.zoomTransform(svg.node());
    const xScaleUpdated = zoom_state.rescaleX(xScale);

    const row = d3.select(this).attr("class").split(/\s+/).filter(s=>s.startsWith("row_"))[0];
    const widget = document.querySelector('.html-widget');
    const root = widget.shadowRoot;

    d.start_date = xScaleUpdated.invert(event.x)
    marker.attr("points", trianglePoints(xScaleUpdated, d, yScale))

    const value_start_date = d3.timeFormat("%Y-%m-%d")(new Date(d.start_date));

    d3.select("#measurement-measurement_date input")
      .property("value", value_start_date)
      .dispatch("change");

    d3.select(root).select(".barLabel." + row)
      .attr("x", labelX(xScaleUpdated, d))
      .text(barLabel(d));

    Shiny.setInputValue(
      "bar_move",
      d,
      {priority: "event"}
    );
  }

  // Elongation right
  function dragRight(event, d) {
    let circleRight = d3.select(this)

    // Update from zoom event
    const zoom_state = d3.zoomTransform(svg.node());
    // Create new scale
    const xScaleUpdated = zoom_state.rescaleX(xScale);

    const initial_x = circleRight.attr("cx")
    // Elongates bar right
    var row = d3.select(this).attr("class").split(/\s+/).filter(s=>s.startsWith("row_"))[0];
    const widget = document.querySelector('.html-widget')
    const root = widget.shadowRoot
    const rectGroupSelection = ".rectGroup." + row
//    console.log(rectGroupSelection)
    let bar = d3.select(root).select(rectGroupSelection)
    const initial_rect_x = +bar.attr("x")
    const newWidth = +bar.attr("width") + event.dx

    circleRight
      .attr("cx", event.x)

    bar
      .attr("width", newWidth)

    showHoverLineAt(event.x)

    // Update
    end_date_position = initial_rect_x + newWidth
    d.end_date = xScaleUpdated.invert(end_date_position)

    let name_end_date;

    if (d.type == "condition_occurrence") {
        name_end_date = "#condition_occurrence-condition_end_date input";
    } else if (d.type == "procedure_occurrence") {
        name_end_date = "#procedure_occurrence-procedure_end_date input";
    } else {
        name_end_date = "#" + d.type + "-" + d.type + "_end_date input";
    }

    const value_end_date = d3.timeFormat("%Y-%m-%d")(new Date(d.end_date));

    d3.select(name_end_date)
      .property("value", value_end_date)
      .dispatch("change");

    d3.select(root).select(".barLabel." + row)
      .text(barLabel(d));

    Shiny.setInputValue(
      "bar_move",
       d,
       {priority: "event"}
       );
  }

  // Elongation left
  function dragLeft(event, d) {
    let circle = d3.select(this)

      // Update from zoom event
      const zoom_state = d3.zoomTransform(svg.node());
      // Create new scale
      const xScaleUpdated = zoom_state.rescaleX(xScale);


    const initial_x = +circle.attr("cx")
    const difference_x = event.x - initial_x

    // Circle elongs rect left
    circle
      .attr("cx", event.x)

    var row = d3.select(this).attr("class").split(/\s+/).filter(s=>s.startsWith("row_"))[0];
    const widget = document.querySelector('.html-widget')
    const root = widget.shadowRoot
    const rectGroupSelection = ".rectGroup." + row
//    console.log(rectGroupSelection)
    let bar = d3.select(root).select(rectGroupSelection)
    const width = +bar.attr("width")

    bar
      .attr('x', event.x)
      .attr("width", width - difference_x)

    showHoverLineAt(event.x)

    // Update
    d.start_date = xScaleUpdated.invert(event.x)
    end_date_position = event.x + width - difference_x
    d.end_date = xScaleUpdated.invert(end_date_position)

    let name_start_date;

    if (d.type == "condition_occurrence") {
      // Branch A: Special naming for conditions
      // We make sure to assign to the same variable names used later
      name_start_date = "#condition_occurrence-condition_start_date input";
      name_end_date = "#condition_occurrence-condition_end_date input";
      
    } else if (d.type == "procedure_occurrence") {
      
      name_start_date = "#procedure_occurrence-procedure_date input";
      name_end_date = "#procedure_occurrence-procedure_end_date input";
    
    } else if (d.type == "measurement") {
            
      name_start_date = "#measurement-measurement_date input";

    } else {
        // Branch B: Standard naming pattern
        name_start_date = "#" + d.type + "-" + d.type + "_start_date input";
        name_end_date = "#" + d.type + "-" + d.type + "_end_date input";
    }
    const value_start_date = d3.timeFormat("%Y-%m-%d")(new Date(d.start_date));

    d3.select(name_start_date)
      .property("value", value_start_date)
      .dispatch("change");

    d3.select(root).select(".barLabel." + row)
      .attr("x", event.x - labelPad)
      .text(barLabel(d));

    // To Shiny
    Shiny.setInputValue(
      "bar_move",
       d,
       {priority: "event"}
       );
  }

  function dragEnd(event, d) {
    hoverLine.style("display", "none");
    bar = d3.select(this)
    bar.style("fill", endColor(d.type))
    Shiny.setInputValue(
      "bar_end",
      d,
      {priority: "event"}
      );
  }

  const moveBar = d3.drag()
    .subject(function(event, d) {

      // Update from zoom event
      const zoom_state = d3.zoomTransform(svg.node());
      // Create new scale
      const xScaleUpdated = zoom_state.rescaleX(xScale);

      return {
        x: xScaleUpdated(new Date(d.start_date)),
      };
    })
    .on("start", dragStart)
    .on("drag", dragMove)
    .on("end", dragEnd)

  const elongLeft = d3.drag()
    .subject(function(event, d) {
      // Update from zoom event
      const zoom_state = d3.zoomTransform(svg.node());
      // Create new scale
      const xScaleUpdated = zoom_state.rescaleX(xScale);

      return {
        x: xScaleUpdated(new Date(d.start_date)),
      };
    })
    .on("start", dragStart)
    .on("drag", dragLeft)
    .on("end", dragEnd)

  const elongRight = d3.drag()
    .subject(function(event, d) {
      // Update from zoom event
      const zoom_state = d3.zoomTransform(svg.node());
      // Create new scale
      const xScaleUpdated = zoom_state.rescaleX(xScale);

      return {
        x: xScaleUpdated(new Date(d.end_date)),
      };
    })
    .on("start", dragStart)
    .on("drag", dragRight)
    .on("end", dragEnd)

  const moveMeasurement = d3.drag()
    .subject(function(event, d) {
      const zoom_state = d3.zoomTransform(svg.node());
      const xScaleUpdated = zoom_state.rescaleX(xScale);

      return {
        x: xScaleUpdated(new Date(d.start_date)),
      };
    })
    .on("start", dragStart)
    .on("drag", dragMeasurement)
    .on("end", dragEnd)

  let g = svg.append("g")

  g.selectAll("rect.rectGroup")
    .data(data.filter(d => d.type !== "measurement"))
    .enter()
    .append("rect")
    .attr("class", d => `rectGroup row_${d.categories}`)
    .attr("x", d => xScale(new Date(d.start_date)))
    .attr("width", d => xScale(new Date(d.end_date)) - xScale(new Date(d.start_date)))
    .attr("y", d => yScale(d.categories))
    .attr("height", barHeight)
    .style("fill", startColor)
    .style("cursor", "pointer")
    .call(moveBar)
    .on("mouseenter", showTooltip)
    .on("mousemove", moveTooltip)
    .on("mouseleave", hideTooltip)
    .attr("transform", `translate(${margin.left},${margin.top + axisPad})`)

  g.selectAll("polygon.measurementMarker")
    .data(data.filter(d => d.type === "measurement"))
    .enter()
    .append("polygon")
    .attr("class", d => `measurementMarker row_${d.categories}`)
    .attr("points", d => trianglePoints(xScale, d, yScale))
    .style("fill", startColor)
    .style("cursor", "move")
    .call(moveMeasurement)
    .on("mouseenter", showTooltip)
    .on("mousemove", moveTooltip)
    .on("mouseleave", hideTooltip)
    .attr("transform", `translate(${margin.left},${margin.top + axisPad})`)

  g.selectAll("text.barLabel")
    .data(data)
    .enter()
    .append("text")
    .attr("class", d => `barLabel row_${d.categories}`)
    .attr("x", d => labelX(xScale, d))
    .attr("y", d => yScale(d.categories) + (barHeight / 2))
    .attr("dy", "0.35em")
    .style("text-anchor", "end")
    .style("font-size", "13px")
    .style("font-family", "sans-serif")
    .style("font-weight", "400")
    .style("fill", "#111")
    .style("paint-order", "stroke")
    .style("stroke", "rgba(255,255,255,0.9)")
    .style("stroke-width", "3px")
    .style("pointer-events", "none")
    .text(d => barLabel(d))
    .attr("transform", `translate(${margin.left},${margin.top + axisPad})`)

  g.selectAll("circle.circleRightGroup")
    .data(data.filter(d => d.type !== "measurement"))
    .enter()
    .append("circle")
    .attr("class", d => `circleRightGroup row_${d.categories}`)
    .attr('r', 5)
    .attr('cx', d => xScale(new Date(d.end_date)))
    .attr("cy", d => yScale(d.categories) + 16)
    .style("fill", startColor)
    .style("cursor", "ew-resize")
    .call(elongRight)
    .attr("transform", `translate(${margin.left},${margin.top + axisPad})`)

  g.selectAll("circle.circleLeftGroup")
    .data(data.filter(d => d.type !== "measurement"))
    .enter()
    .append("circle")
    .attr("class", d => `circleLeftGroup row_${d.categories}`)
    .attr('r', 5)
    .attr('cx', d => xScale(new Date(d.start_date)))
    .attr("cy", d => yScale(d.categories) + 16)
    .style("fill", startColor)
    .style("cursor", "ew-resize")
    .call(elongLeft)
    .attr("transform", `translate(${margin.left},${margin.top + axisPad})`)

  const oneDay = 24 * 60 * 60 * 1000;
  const totalMilliseconds = domainEnd - domainStart;
  const maxZoom = totalMilliseconds / oneDay;


 var zoom = d3.zoom()
    .scaleExtent([1, maxZoom])
    .translateExtent([[0, 0], [width, height]])
    .on("zoom", zooming)

  svg
    .call(zoom)
    
  svg
  .call(
    zoom.transform,
    currentTransform
    );


});
