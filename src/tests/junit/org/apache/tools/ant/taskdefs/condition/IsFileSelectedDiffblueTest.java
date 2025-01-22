package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.junit.Test;

public class IsFileSelectedDiffblueTest {
  /**
   * Test {@link IsFileSelected#validate()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFileSelected#validate()}
   */
  @Test
  public void testValidate_givenIsFileSelected_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsFileSelected()).validate());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.setProject(project);
    isFileSelected.appendSelector(new AndSelector());
    isFileSelected.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());

    // Act and Assert
    assertTrue(isFileSelected.eval());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.setProject(project);
    isFileSelected.appendSelector(new AndSelector());
    isFileSelected.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "Adding reference: ").toFile());

    // Act and Assert
    assertTrue(isFileSelected.eval());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval_givenAndSelectorAddOrOrSelector_thenReturnFalse() {
    // Arrange
    AndSelector selector = new AndSelector();
    selector.addOr(new OrSelector());

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.setProject(project);
    isFileSelected.appendSelector(selector);
    isFileSelected.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(isFileSelected.eval());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor) appendSelector {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval_givenIsFileSelectedAppendSelectorAndSelector_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.setProject(project);
    isFileSelected.appendSelector(new AndSelector());
    isFileSelected.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(isFileSelected.eval());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval_givenIsFileSelected_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsFileSelected()).eval());
  }

  /**
   * Test {@link IsFileSelected#eval()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFileSelected#eval()}
   */
  @Test
  public void testEval_thenThrowBuildException() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> isFileSelected.eval());
  }

  /**
   * Test new {@link IsFileSelected} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsFileSelected}
   */
  @Test
  public void testNewIsFileSelected() {
    // Arrange and Act
    IsFileSelected actualIsFileSelected = new IsFileSelected();

    // Assert
    Location location = actualIsFileSelected.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsFileSelected.getDescription());
    assertNull(actualIsFileSelected.getProject());
    assertNull(actualIsFileSelected.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualIsFileSelected.isReference());
  }
}
