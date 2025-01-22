package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.ExecutorTest;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ParserSupportsDiffblueTest {
  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenJavaLangObject_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(null);
    parserSupports.setProperty("foo");
    parserSupports.setValue("foo");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Feature is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsFeatureIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature(null);
    parserSupports.setProperty("foo");
    parserSupports.setValue("foo");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsProjectIsProject_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(new Project());
    parserSupports.setFeature(null);
    parserSupports.setProperty("foo");
    parserSupports.setValue("foo");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Property is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsPropertyIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature("foo");
    parserSupports.setProperty(null);
    parserSupports.setValue("foo");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Value is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsValueIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature("foo");
    parserSupports.setProperty(null);
    parserSupports.setValue(null);

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Value is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsValueIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature(null);
    parserSupports.setProperty("foo");
    parserSupports.setValue(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Value is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsValueIsNull_thenThrowBuildException2() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature("foo");
    parserSupports.setProperty("foo");
    parserSupports.setValue(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Value is {@code on}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsValueIsOn_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature("foo");
    parserSupports.setProperty(null);
    parserSupports.setValue("on");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Value is {@code yes}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupportsValueIsYes_thenReturnFalse() throws BuildException {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature("foo");
    parserSupports.setProperty(null);
    parserSupports.setValue("yes");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenParserSupports_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ParserSupports()).eval());
  }

  /**
   * Test {@link ParserSupports#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(null);
    parserSupports.setProperty("foo");
    parserSupports.setValue("foo");

    // Act and Assert
    assertFalse(parserSupports.eval());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenJavaLangObject_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("on", typeClass);
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Feature is {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenParserSupportsFeatureIsTrueToString_thenReturnFalse() {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenParserSupportsProjectIsProject_thenReturnFalse() {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(new Project());
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenProjectAddBuildListenerDefaultLogger_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalFeature()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalFeature()}
   */
  @Test
  public void testEvalFeature_givenProjectAddBuildListenerExecutorTest_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setFeature(Boolean.TRUE.toString());

    // Act and Assert
    assertFalse(parserSupports.evalFeature());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenJavaLangObject_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenParserSupportsProjectIsProject_thenReturnFalse() {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(new Project());
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@link ParserSupports} (default constructor) Property is {@code Property}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenParserSupportsPropertyIsProperty_thenReturnFalse() {
    // Arrange
    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenProjectAddBuildListenerDefaultLogger_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test {@link ParserSupports#evalProperty()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParserSupports#evalProperty()}
   */
  @Test
  public void testEvalProperty_givenProjectAddBuildListenerExecutorTest_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    ParserSupports parserSupports = new ParserSupports();
    parserSupports.setProject(project);
    parserSupports.setProperty("Property");

    // Act and Assert
    assertFalse(parserSupports.evalProperty());
  }

  /**
   * Test new {@link ParserSupports} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ParserSupports}
   */
  @Test
  public void testNewParserSupports() {
    // Arrange and Act
    ParserSupports actualParserSupports = new ParserSupports();

    // Assert
    Location location = actualParserSupports.getLocation();
    assertNull(location.getFileName());
    assertNull(actualParserSupports.getDescription());
    assertNull(actualParserSupports.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
