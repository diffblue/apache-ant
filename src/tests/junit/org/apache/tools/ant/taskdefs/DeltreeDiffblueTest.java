package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class DeltreeDiffblueTest {
  /**
   * Test {@link Deltree#execute()}.
   * <p>
   * Method under test: {@link Deltree#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("DEPRECATED - The deltree task is deprecated.  Use delete instead."));

    Deltree deltree = new Deltree();
    deltree.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> deltree.execute());
  }

  /**
   * Test {@link Deltree#execute()}.
   * <ul>
   *   <li>Given {@link Deltree} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Deltree#execute()}
   */
  @Test
  public void testExecute_givenDeltreeProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Deltree deltree = new Deltree();
    deltree.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> deltree.execute());
  }

  /**
   * Test {@link Deltree#execute()}.
   * <ul>
   *   <li>Given {@link Deltree} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Deltree#execute()}
   */
  @Test
  public void testExecute_givenDeltree_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Deltree()).execute());
  }

  /**
   * Test {@link Deltree#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Deltree#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("dir attribute must be set!", typeClass);
    project.addBuildListener(new AntClassLoader());

    Deltree deltree = new Deltree();
    deltree.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> deltree.execute());
  }

  /**
   * Test {@link Deltree#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Deltree#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Deltree deltree = new Deltree();
    deltree.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> deltree.execute());
  }

  /**
   * Test new {@link Deltree} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Deltree}
   */
  @Test
  public void testNewDeltree() {
    // Arrange and Act
    Deltree actualDeltree = new Deltree();

    // Assert
    Location location = actualDeltree.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDeltree.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualDeltree.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualDeltree.getTaskName());
    assertNull(actualDeltree.getTaskType());
    assertNull(actualDeltree.getProject());
    assertNull(actualDeltree.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualDeltree, runtimeConfigurableWrapper.getProxy());
  }
}
