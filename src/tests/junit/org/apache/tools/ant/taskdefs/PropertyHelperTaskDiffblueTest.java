package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class PropertyHelperTaskDiffblueTest {
  /**
   * Test {@link PropertyHelperTask#createDelegate()}.
   * <p>
   * Method under test: {@link PropertyHelperTask#createDelegate()}
   */
  @Test
  public void testCreateDelegate() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelperTask()).createDelegate().getRefid());
  }

  /**
   * Test {@link PropertyHelperTask#execute()}.
   * <ul>
   *   <li>Given {@link PropertyHelperTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelperTask#execute()}
   */
  @Test
  public void testExecute_givenPropertyHelperTask() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyHelperTask()).execute());
  }

  /**
   * Test {@link PropertyHelperTask#execute()}.
   * <ul>
   *   <li>Given {@link PropertyHelperTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelperTask#execute()}
   */
  @Test
  public void testExecute_givenPropertyHelperTaskProjectIsProject() throws BuildException {
    // Arrange
    PropertyHelperTask propertyHelperTask = new PropertyHelperTask();
    propertyHelperTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyHelperTask.execute());
  }

  /**
   * Test new {@link PropertyHelperTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PropertyHelperTask}
   */
  @Test
  public void testNewPropertyHelperTask() {
    // Arrange and Act
    PropertyHelperTask actualPropertyHelperTask = new PropertyHelperTask();

    // Assert
    Location location = actualPropertyHelperTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPropertyHelperTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualPropertyHelperTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualPropertyHelperTask.getTaskName());
    assertNull(actualPropertyHelperTask.getTaskType());
    assertNull(actualPropertyHelperTask.getProject());
    assertNull(actualPropertyHelperTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualPropertyHelperTask, runtimeConfigurableWrapper.getProxy());
  }
}
