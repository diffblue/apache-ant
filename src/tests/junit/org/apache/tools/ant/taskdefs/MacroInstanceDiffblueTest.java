package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.MacroInstance.Element;
import org.junit.Test;

public class MacroInstanceDiffblueTest {
  /**
   * Test {@link MacroInstance#createDynamicElement(String)}.
   * <p>
   * Method under test: {@link MacroInstance#createDynamicElement(String)}
   */
  @Test
  public void testCreateDynamicElement() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MacroInstance()).createDynamicElement(Manifest.ATTRIBUTE_NAME));
  }

  /**
   * Test Element {@link Element#addTask(Task)}.
   * <p>
   * Method under test: {@link Element#addTask(Task)}
   */
  @Test
  public void testElementAddTask() {
    // Arrange
    Element element = new Element();
    TaskAdapter nestedTask = new TaskAdapter();

    // Act
    element.addTask(nestedTask);

    // Assert
    List<Task> unknownElements = element.getUnknownElements();
    assertEquals(1, unknownElements.size());
    assertSame(nestedTask, unknownElements.get(0));
  }

  /**
   * Test Element getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Element}
   *   <li>{@link Element#getUnknownElements()}
   * </ul>
   */
  @Test
  public void testElementGettersAndSetters() {
    // Arrange, Act and Assert
    assertTrue((new Element()).getUnknownElements().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link MacroInstance}
   *   <li>{@link MacroInstance#setMacroDef(MacroDef)}
   *   <li>{@link MacroInstance#addText(String)}
   *   <li>{@link MacroInstance#getMacroDef()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    MacroInstance actualMacroInstance = new MacroInstance();
    MacroDef macroDef = new MacroDef();
    actualMacroInstance.setMacroDef(macroDef);
    actualMacroInstance.addText("Text");
    MacroDef actualMacroDef = actualMacroInstance.getMacroDef();

    // Assert
    Location location = actualMacroInstance.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMacroInstance.getDescription());
    assertNull(actualMacroInstance.getTaskName());
    assertNull(actualMacroInstance.getTaskType());
    assertNull(actualMacroInstance.getProject());
    assertNull(actualMacroInstance.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(macroDef, actualMacroDef);
  }
}
