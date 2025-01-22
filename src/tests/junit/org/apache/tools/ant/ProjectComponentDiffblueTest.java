package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ProjectComponentDiffblueTest {
  /**
   * Test {@link ProjectComponent#setProject(Project)}.
   * <p>
   * Method under test: {@link ProjectComponent#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    Project project = new Project();

    // Act
    taskAdapter.setProject(project);

    // Assert
    assertSame(project, taskAdapter.getProject());
  }

  /**
   * Test {@link ProjectComponent#getProject()}.
   * <p>
   * Method under test: {@link ProjectComponent#getProject()}
   */
  @Test
  public void testGetProject() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getProject());
  }

  /**
   * Test {@link ProjectComponent#getLocation()}.
   * <p>
   * Method under test: {@link ProjectComponent#getLocation()}
   */
  @Test
  public void testGetLocation() {
    // Arrange and Act
    Location actualLocation = (new TaskAdapter()).getLocation();

    // Assert
    assertSame(actualLocation.UNKNOWN_LOCATION, actualLocation);
  }

  /**
   * Test {@link ProjectComponent#setDescription(String)}.
   * <p>
   * Method under test: {@link ProjectComponent#setDescription(String)}
   */
  @Test
  public void testSetDescription() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();

    // Act
    taskAdapter.setDescription("The characteristics of someone or something");

    // Assert
    assertEquals("The characteristics of someone or something", taskAdapter.getDescription());
  }

  /**
   * Test {@link ProjectComponent#getDescription()}.
   * <p>
   * Method under test: {@link ProjectComponent#getDescription()}
   */
  @Test
  public void testGetDescription() {
    // Arrange, Act and Assert
    assertNull((new TaskAdapter()).getDescription());
  }

  /**
   * Test {@link ProjectComponent#clone()}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath}.</li>
   *   <li>Then return {@link Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectComponent#clone()}
   */
  @Test
  public void testClone_givenSystemBootClasspath_thenReturnPath() {
    // Arrange and Act
    Object actualCloneResult = Path.systemBootClasspath.clone();

    // Assert
    assertTrue(actualCloneResult instanceof Path);
    assertNull(((Path) actualCloneResult).getRefid());
    assertEquals(0, ((Path) actualCloneResult).size());
    assertFalse(((Path) actualCloneResult).isReference());
    assertTrue(((Path) actualCloneResult).isEmpty());
  }

  /**
   * Test {@link ProjectComponent#clone()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return {@link TaskAdapter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectComponent#clone()}
   */
  @Test
  public void testClone_givenTaskAdapter_thenReturnTaskAdapter() throws CloneNotSupportedException {
    // Arrange and Act
    Object actualCloneResult = (new TaskAdapter()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof TaskAdapter);
    assertNull(((TaskAdapter) actualCloneResult).getProxy());
    RuntimeConfigurable runtimeConfigurableWrapper = ((TaskAdapter) actualCloneResult).getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(((TaskAdapter) actualCloneResult).getTaskName());
    assertNull(((TaskAdapter) actualCloneResult).getTaskType());
    assertNull(((TaskAdapter) actualCloneResult).getProject());
    assertNull(((TaskAdapter) actualCloneResult).getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertFalse(((TaskAdapter) actualCloneResult).isInvalid());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCloneResult, runtimeConfigurableWrapper.getProxy());
    assertSame(runtimeConfigurableWrapper, ((TaskAdapter) actualCloneResult).getWrapper());
  }
}
