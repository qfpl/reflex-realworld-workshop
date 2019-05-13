function Link(el)
  if string.match(el.target, "http") then
  else
    el.target = string.gsub(el.target, "%.md", ".html")
  end
  return el
end
