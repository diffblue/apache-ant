package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ClassloaderDiffblueTest {
  /**
   * Test {@link Classloader#createClasspath()}.
   * <ul>
   *   <li>Given {@link Classloader} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Classloader#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenClassloader_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new Classloader()).createClasspath();

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
   * Test {@link Classloader#createClasspath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Classloader#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnProjectIsProject() {
    // Arrange
    Classloader classloader = new Classloader();
    Project project = new Project();
    classloader.setClasspath(new Path(project));

    // Act and Assert
    assertSame(project, classloader.createClasspath().getProject());
  }

  /**
   * Test new {@link Classloader} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Classloader}
   */
  @Test
  public void testNewClassloader() {
    // Arrange and Act
    Classloader actualClassloader = new Classloader();

    // Assert
    Location location = actualClassloader.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClassloader.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualClassloader.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualClassloader.getTaskName());
    assertNull(actualClassloader.getTaskType());
    assertNull(actualClassloader.getProject());
    assertNull(actualClassloader.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualClassloader, runtimeConfigurableWrapper.getProxy());
  }
}
