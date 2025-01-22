package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class HasMethodDiffblueTest {
  /**
   * Test {@link HasMethod#createClasspath()}.
   * <ul>
   *   <li>Given {@link HasMethod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenHasMethodProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    HasMethod hasMethod = new HasMethod();
    Project project = new Project();
    hasMethod.setProject(project);

    // Act and Assert
    assertSame(project, hasMethod.createClasspath().getProject());
  }

  /**
   * Test {@link HasMethod#createClasspath()}.
   * <ul>
   *   <li>Given {@link HasMethod} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenHasMethod_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new HasMethod()).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link HasMethod} (default constructor) Classname is {@code No classname defined}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenHasMethodClassnameIsNoClassnameDefined_thenThrowBuildException() throws BuildException {
    // Arrange
    HasMethod hasMethod = new HasMethod();
    hasMethod.setClassname("No classname defined");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasMethod.eval());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link HasMethod} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenHasMethodProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    HasMethod hasMethod = new HasMethod();
    hasMethod.setProject(new Project());
    hasMethod.setIgnoreSystemClasses(true);
    hasMethod.setClassname("No classname defined");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasMethod.eval());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link HasMethod} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenHasMethod_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new HasMethod()).eval());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    HasMethod hasMethod = new HasMethod();
    hasMethod.setProject(project);
    hasMethod.setIgnoreSystemClasses(true);
    hasMethod.setClassname("No classname defined");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasMethod.eval());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    HasMethod hasMethod = new HasMethod();
    hasMethod.setProject(project);
    hasMethod.setIgnoreSystemClasses(true);
    hasMethod.setClassname("No classname defined");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasMethod.eval());
  }

  /**
   * Test {@link HasMethod#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addDataTypeDefinition {@code Finding class} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasMethod#eval()}
   */
  @Test
  public void testEval_givenProjectAddDataTypeDefinitionFindingClassAndObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);
    project.addBuildListener(new AntClassLoader());

    HasMethod hasMethod = new HasMethod();
    hasMethod.setProject(project);
    hasMethod.setIgnoreSystemClasses(true);
    hasMethod.setClassname("No classname defined");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasMethod.eval());
  }

  /**
   * Test new {@link HasMethod} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link HasMethod}
   */
  @Test
  public void testNewHasMethod() {
    // Arrange and Act
    HasMethod actualHasMethod = new HasMethod();

    // Assert
    Location location = actualHasMethod.getLocation();
    assertNull(location.getFileName());
    assertNull(actualHasMethod.getDescription());
    assertNull(actualHasMethod.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
