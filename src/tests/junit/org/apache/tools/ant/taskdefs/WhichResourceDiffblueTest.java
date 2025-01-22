package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class WhichResourceDiffblueTest {
  /**
   * Test {@link WhichResource#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path cp = new Path(project);
    cp.addFileset(new FileSet());

    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(cp);
    Path cp2 = Path.systemBootClasspath;
    cp2.setProject(null);

    // Act
    whichResource.setClasspath(cp2);

    // Assert
    assertSame(project, cp2.getProject());
  }

  /**
   * Test {@link WhichResource#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(new Path(null));
    Path cp = Path.systemBootClasspath;
    cp.setProject(null);

    // Act
    whichResource.setClasspath(cp);

    // Assert that nothing has changed
    assertNull(cp.getProject());
  }

  /**
   * Test {@link WhichResource#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    WhichResource whichResource = new WhichResource();
    Project project = new Project();
    whichResource.setClasspath(new Path(project));
    Path cp = Path.systemBootClasspath;
    cp.setProject(null);

    // Act
    whichResource.setClasspath(cp);

    // Assert
    assertSame(project, cp.getProject());
  }

  /**
   * Test {@link WhichResource#createClasspath()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenWhichResource_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new WhichResource()).createClasspath();

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
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor) Class is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_givenWhichResourceClassIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(null);
    whichResource.setClass("foo");
    whichResource.setResource("/");
    whichResource.setProperty(null);
    whichResource.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> whichResource.execute());
  }

  /**
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor) Property is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_givenWhichResourcePropertyIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(null);
    whichResource.setClass(null);
    whichResource.setResource("/");
    whichResource.setProperty(null);
    whichResource.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> whichResource.execute());
  }

  /**
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor) Resource is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_givenWhichResourceResourceIsFoo() throws BuildException {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(null);
    whichResource.setClass(null);
    whichResource.setResource("foo");
    whichResource.setProperty("foo");
    whichResource.setProject(new Project());

    // Act
    whichResource.execute();

    // Assert that nothing has changed
    Project project = whichResource.getProject();
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    assertEquals(1, project.getPropertyNames().size());
    assertTrue(properties.containsKey("basedir"));
  }

  /**
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor) Resource is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_givenWhichResourceResourceIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(null);
    whichResource.setClass("foo");
    whichResource.setResource(null);
    whichResource.setProperty(null);
    whichResource.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> whichResource.execute());
  }

  /**
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Given {@link WhichResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_givenWhichResource_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new WhichResource()).execute());
  }

  /**
   * Test {@link WhichResource#execute()}.
   * <ul>
   *   <li>Then {@link WhichResource} (default constructor) Project Properties size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link WhichResource#execute()}
   */
  @Test
  public void testExecute_thenWhichResourceProjectPropertiesSizeIsOne() throws BuildException {
    // Arrange
    WhichResource whichResource = new WhichResource();
    whichResource.setClasspath(null);
    whichResource.setClass("foo");
    whichResource.setResource(null);
    whichResource.setProperty("foo");
    whichResource.setProject(new Project());

    // Act
    whichResource.execute();

    // Assert that nothing has changed
    Project project = whichResource.getProject();
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    assertEquals(1, project.getPropertyNames().size());
    assertTrue(properties.containsKey("basedir"));
  }

  /**
   * Test new {@link WhichResource} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link WhichResource}
   */
  @Test
  public void testNewWhichResource() {
    // Arrange and Act
    WhichResource actualWhichResource = new WhichResource();

    // Assert
    Location location = actualWhichResource.getLocation();
    assertNull(location.getFileName());
    assertNull(actualWhichResource.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualWhichResource.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualWhichResource.getTaskName());
    assertNull(actualWhichResource.getTaskType());
    assertNull(actualWhichResource.getProject());
    assertNull(actualWhichResource.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualWhichResource, runtimeConfigurableWrapper.getProxy());
  }
}
