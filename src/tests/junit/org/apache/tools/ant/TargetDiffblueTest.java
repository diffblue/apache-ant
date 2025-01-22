package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.apache.tools.ant.taskdefs.Available;
import org.apache.tools.ant.taskdefs.condition.Condition;
import org.junit.Test;

public class TargetDiffblueTest {
  /**
   * Test {@link Target#Target()}.
   * <p>
   * Method under test: {@link Target#Target()}
   */
  @Test
  public void testNewTarget() {
    // Arrange and Act
    Target actualTarget = new Target();

    // Assert
    Location location = actualTarget.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTarget.getDescription());
    assertNull(actualTarget.getIf());
    assertNull(actualTarget.getName());
    assertNull(actualTarget.getUnless());
    assertNull(actualTarget.toString());
    assertNull(actualTarget.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualTarget.getTasks().length);
  }

  /**
   * Test {@link Target#Target(Target)}.
   * <ul>
   *   <li>When {@link Target#Target()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#Target(Target)}
   */
  @Test
  public void testNewTarget_whenTarget_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Target actualTarget = new Target(new Target());

    // Assert
    Location location = actualTarget.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTarget.getDescription());
    assertNull(actualTarget.getIf());
    assertNull(actualTarget.getName());
    assertNull(actualTarget.getUnless());
    assertNull(actualTarget.toString());
    assertNull(actualTarget.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualTarget.getTasks().length);
  }

  /**
   * Test {@link Target#setDepends(String)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@code ,}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setDepends(String)}
   */
  @Test
  public void testSetDepends_givenTarget_whenComma_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Target()).setDepends(","));
  }

  /**
   * Test {@link Target#setDepends(String)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@code depends,}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setDepends(String)}
   */
  @Test
  public void testSetDepends_givenTarget_whenDepends_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Target()).setDepends("depends,"));
  }

  /**
   * Test {@link Target#parseDepends(String, String, String)}.
   * <ul>
   *   <li>When {@code ,}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#parseDepends(String, String, String)}
   */
  @Test
  public void testParseDepends_whenComma_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Target.parseDepends(",", "Target Name", "Attribute Name"));
  }

  /**
   * Test {@link Target#parseDepends(String, String, String)}.
   * <ul>
   *   <li>When {@code Depends}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#parseDepends(String, String, String)}
   */
  @Test
  public void testParseDepends_whenDepends_thenReturnSizeIsOne() {
    // Arrange and Act
    List<String> actualParseDependsResult = Target.parseDepends("Depends", "Target Name", "Attribute Name");

    // Assert
    assertEquals(1, actualParseDependsResult.size());
    assertEquals("Depends", actualParseDependsResult.get(0));
  }

  /**
   * Test {@link Target#parseDepends(String, String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#parseDepends(String, String, String)}
   */
  @Test
  public void testParseDepends_whenEmptyString_thenReturnEmpty() {
    // Arrange and Act
    List<String> actualParseDependsResult = Target.parseDepends("", "Target Name", "Attribute Name");

    // Assert
    assertTrue(actualParseDependsResult.isEmpty());
  }

  /**
   * Test {@link Target#addTask(Task)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#addTask(Task)}
   */
  @Test
  public void testAddTask_givenTarget_thenArrayLengthIsOne() {
    // Arrange
    Target target = new Target();
    TaskAdapter task = new TaskAdapter();

    // Act
    target.addTask(task);

    // Assert
    Task[] tasks = target.getTasks();
    assertEquals(1, tasks.length);
    assertSame(task, tasks[0]);
  }

  /**
   * Test {@link Target#getTasks()}.
   * <ul>
   *   <li>Given {@link Target#Target()} addDataType {@link RuntimeConfigurable#RuntimeConfigurable(Object, String)} with {@code Proxy} and {@code Element Tag}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getTasks()}
   */
  @Test
  public void testGetTasks_givenTargetAddDataTypeRuntimeConfigurableWithProxyAndElementTag() {
    // Arrange
    Target target = new Target();
    target.addDataType(new RuntimeConfigurable("Proxy", "Element Tag"));
    TaskAdapter task = new TaskAdapter();
    target.addTask(task);

    // Act
    Task[] actualTasks = target.getTasks();

    // Assert
    assertEquals(1, actualTasks.length);
    assertSame(task, actualTasks[0]);
  }

  /**
   * Test {@link Target#getTasks()}.
   * <ul>
   *   <li>Given {@link Target#Target()} addTask {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getTasks()}
   */
  @Test
  public void testGetTasks_givenTargetAddTaskTaskAdapter_thenReturnArrayLengthIsOne() {
    // Arrange
    Target target = new Target();
    TaskAdapter task = new TaskAdapter();
    target.addTask(task);

    // Act
    Task[] actualTasks = target.getTasks();

    // Assert
    assertEquals(1, actualTasks.length);
    assertSame(task, actualTasks[0]);
  }

  /**
   * Test {@link Target#getTasks()}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getTasks()}
   */
  @Test
  public void testGetTasks_givenTarget_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Target()).getTasks().length);
  }

  /**
   * Test {@link Target#dependsOn(String)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#dependsOn(String)}
   */
  @Test
  public void testDependsOn_givenTarget_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Target()).dependsOn("Other"));
  }

  /**
   * Test {@link Target#setIf(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()} If is {@link Available} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Target#Target()} If is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setIf(String)}
   */
  @Test
  public void testSetIfWithProperty_givenTargetIfIsAvailable_whenNull_thenTargetIfIsNull() {
    // Arrange
    Target target = new Target();
    target.setIf(new Available());

    // Act
    target.setIf((String) null);

    // Assert that nothing has changed
    assertNull(target.getIf());
  }

  /**
   * Test {@link Target#setIf(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()} If is {@code null}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Target#Target()} If is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setIf(String)}
   */
  @Test
  public void testSetIfWithProperty_givenTargetIfIsNull_whenNull_thenTargetIfIsNull() {
    // Arrange
    Target target = new Target();
    target.setIf((Condition) null);

    // Act
    target.setIf((String) null);

    // Assert that nothing has changed
    assertNull(target.getIf());
  }

  /**
   * Test {@link Target#setIf(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@code Property}.</li>
   *   <li>Then {@link Target#Target()} If is {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setIf(String)}
   */
  @Test
  public void testSetIfWithProperty_givenTarget_whenProperty_thenTargetIfIsProperty() {
    // Arrange
    Target target = new Target();

    // Act
    target.setIf("Property");

    // Assert
    assertEquals("Property", target.getIf());
  }

  /**
   * Test {@link Target#setUnless(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()} Unless is {@link Available} (default constructor).</li>
   *   <li>Then {@link Target#Target()} Unless is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setUnless(String)}
   */
  @Test
  public void testSetUnlessWithProperty_givenTargetUnlessIsAvailable_thenTargetUnlessIsNull() {
    // Arrange
    Target target = new Target();
    target.setUnless(new Available());

    // Act
    target.setUnless((String) null);

    // Assert that nothing has changed
    assertNull(target.getUnless());
  }

  /**
   * Test {@link Target#setUnless(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()} Unless is {@code null}.</li>
   *   <li>Then {@link Target#Target()} Unless is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setUnless(String)}
   */
  @Test
  public void testSetUnlessWithProperty_givenTargetUnlessIsNull_thenTargetUnlessIsNull() {
    // Arrange
    Target target = new Target();
    target.setUnless((Condition) null);

    // Act
    target.setUnless((String) null);

    // Assert that nothing has changed
    assertNull(target.getUnless());
  }

  /**
   * Test {@link Target#setUnless(String)} with {@code property}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@code Property}.</li>
   *   <li>Then {@link Target#Target()} Unless is {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#setUnless(String)}
   */
  @Test
  public void testSetUnlessWithProperty_givenTarget_whenProperty_thenTargetUnlessIsProperty() {
    // Arrange
    Target target = new Target();

    // Act
    target.setUnless("Property");

    // Assert
    assertEquals("Property", target.getUnless());
  }

  /**
   * Test {@link Target#getIf()}.
   * <ul>
   *   <li>Given {@link Target#Target()} If is {@code Property}.</li>
   *   <li>Then return {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getIf()}
   */
  @Test
  public void testGetIf_givenTargetIfIsProperty_thenReturnProperty() {
    // Arrange
    Target target = new Target();
    target.setIf("Property");

    // Act and Assert
    assertEquals("Property", target.getIf());
  }

  /**
   * Test {@link Target#getIf()}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getIf()}
   */
  @Test
  public void testGetIf_givenTarget_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Target()).getIf());
  }

  /**
   * Test {@link Target#getUnless()}.
   * <ul>
   *   <li>Given {@link Target#Target()} Unless is {@code Property}.</li>
   *   <li>Then return {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getUnless()}
   */
  @Test
  public void testGetUnless_givenTargetUnlessIsProperty_thenReturnProperty() {
    // Arrange
    Target target = new Target();
    target.setUnless("Property");

    // Act and Assert
    assertEquals("Property", target.getUnless());
  }

  /**
   * Test {@link Target#getUnless()}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Target#getUnless()}
   */
  @Test
  public void testGetUnless_givenTarget_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Target()).getUnless());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Target#setDescription(String)}
   *   <li>{@link Target#setLocation(Location)}
   *   <li>{@link Target#setName(String)}
   *   <li>{@link Target#setProject(Project)}
   *   <li>{@link Target#getDescription()}
   *   <li>{@link Target#getLocation()}
   *   <li>{@link Target#getName()}
   *   <li>{@link Target#getProject()}
   *   <li>{@link Target#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Target target = new Target();

    // Act
    target.setDescription("The characteristics of someone or something");
    target.setLocation(Location.UNKNOWN_LOCATION);
    target.setName("Name");
    Project project = new Project();
    target.setProject(project);
    String actualDescription = target.getDescription();
    Location actualLocation = target.getLocation();
    String actualName = target.getName();
    Project actualProject = target.getProject();

    // Assert
    assertEquals("Name", actualName);
    assertEquals("Name", target.toString());
    assertEquals("The characteristics of someone or something", actualDescription);
    assertSame(project, actualProject);
    assertSame(actualLocation.UNKNOWN_LOCATION, actualLocation);
  }
}
