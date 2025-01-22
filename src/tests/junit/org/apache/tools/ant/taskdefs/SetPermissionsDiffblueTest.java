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
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class SetPermissionsDiffblueTest {
  /**
   * Test {@link SetPermissions#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.ComponentHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SetPermissions#execute()}
   */
  @Test
  public void testExecute_givenProjectAddReferenceAntComponentHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.ComponentHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    DirSet rc = new DirSet();
    rc.setProject(project);

    SetPermissions setPermissions = new SetPermissions();
    setPermissions.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> setPermissions.execute());
  }

  /**
   * Test {@link SetPermissions#execute()}.
   * <ul>
   *   <li>Given {@link SetPermissions} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SetPermissions#execute()}
   */
  @Test
  public void testExecute_givenSetPermissions_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SetPermissions()).execute());
  }

  /**
   * Test {@link SetPermissions#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SetPermissions#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() {
    // Arrange
    SetPermissions setPermissions = new SetPermissions();
    setPermissions.add(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> setPermissions.execute());
  }

  /**
   * Test new {@link SetPermissions} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SetPermissions}
   */
  @Test
  public void testNewSetPermissions() {
    // Arrange and Act
    SetPermissions actualSetPermissions = new SetPermissions();

    // Assert
    Location location = actualSetPermissions.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSetPermissions.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSetPermissions.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSetPermissions.getTaskName());
    assertNull(actualSetPermissions.getTaskType());
    assertNull(actualSetPermissions.getProject());
    assertNull(actualSetPermissions.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSetPermissions, runtimeConfigurableWrapper.getProxy());
  }
}
