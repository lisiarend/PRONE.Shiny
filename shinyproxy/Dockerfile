FROM openjdk:11-jre

RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-3.0.2.jar -O /opt/shinyproxy/shinyproxy.jar
COPY application.yml /opt/shinyproxy/application.yml
COPY favicon.png /opt/shinyproxy/favicon.png
WORKDIR /opt/shinyproxy/
CMD ["java", "-jar", "/opt/shinyproxy/shinyproxy.jar"]