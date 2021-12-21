#필요 라이브러리
library('reticulate')

#consoleClear 함수 정의
consoleClear <- function(){
    #python 실행 코드드
    reticulate::py_run_file('scripts/consoleClear.py')
}

showError <- function(msg, vectors){
    cat(msg)
    print(vectors)
    cat("이들 중 선택해주세요\n")
}

# 존재하는 선택지 정의
tagNames = c("django", "flask", "jinja2", "jupyternotebook", "numpy", "pandas", "pip", "pygame", "pyinstaller", "pyqt", "pyserial", "python", "pytorch", "selenium", "sympy", "tensorflow")
types = c("year", "month")

#타입 선택
type="year"
tagName = "tensorflow"

if(!tagName %in% tagNames){
    #콘솔 정리
    consoleClear()
    #print
    showError(sprintf("%s는 존재하지 않는 태그이거나 csv파일로 변경하지 않은 파일입니다.\n", tagName), tagNames)
    
} else if( !type %in% types){
    #콘솔 정리
    consoleClear()
    #print
    showError(sprintf("%s은 존재하지 않는 타입입니다.\n", type), types)
    
} else{
    #csv가져오기
    csv <- read.csv(sprintf("resources/%s/%sTagResource.csv", type, tagName))
    
    #x값, 연도/달 정의
    if(type=="month"){
        x <- as.Date(as.character(csv$year * 100 + 01), '%Y%m%d')    
        xlab = "달(월)"
    }
    else if(type == "year"){
        x <- as.Date(as.character(csv$year * 10000 + 0101), '%Y%m%d')   
        xlab = "연도(년)"
    }
    
    #y값 정의
    y <- csv$count
    
    #점그래프 그리기
    plot(x, y, 
         pch=19, 
         col="blue", 
         xlab=xlab, ylab=sprintf("%s 태그 개수", tagName), main=sprintf("%s별 %s 태그 개수", xlab, tagName
        ))
    
    #분석
    m <- lm(y~x)
    
    #선형회귀
    abline(m, col="red")
    #콘솔 정리
    consoleClear()
    #종료 메세지
    cat(sprintf("%s 태그에 관한 %s별 그래프를 성공적으로 그렸습니다.\n", tagName, xlab))
    
}
