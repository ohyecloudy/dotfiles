(setq org-taskjuggler-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='

  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns name, start, end, chart {scale day width 2000}
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}"))

;; Windows + git-sdk bash 환경에서 `tj3`(sh polyglot wrapper)는
;; 네이티브 ruby.exe에 POSIX 경로($0)를 넘겨 LoadError 발생.
;; `tj3.bat`은 Windows 절대경로를 넘겨 정상 동작하므로 이를 사용.
(when (eq system-type 'windows-nt)
  (setq org-taskjuggler-process-command
        "tj3.bat --silent --no-color --output-dir %o %f"))
