package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ClearCaseDiffblueTest {
  /**
   * Test {@link ClearCase#setClearToolDir(String)}.
   * <ul>
   *   <li>When {@code Dir}.</li>
   *   <li>Then {@link CCCheckin} (default constructor) ClearToolCommand is {@code Dir/cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#setClearToolDir(String)}
   */
  @Test
  public void testSetClearToolDir_whenDir_thenCCCheckinClearToolCommandIsDirCleartool() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setClearToolDir("Dir");

    // Assert
    assertEquals("Dir/cleartool", ccCheckin.getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#setClearToolDir(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CCCheckin} (default constructor) ClearToolCommand is {@code cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#setClearToolDir(String)}
   */
  @Test
  public void testSetClearToolDir_whenEmptyString_thenCCCheckinClearToolCommandIsCleartool() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setClearToolDir("");

    // Assert that nothing has changed
    assertEquals("cleartool", ccCheckin.getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#setClearToolDir(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then {@link CCCheckin} (default constructor) ClearToolCommand is {@code /cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#setClearToolDir(String)}
   */
  @Test
  public void testSetClearToolDir_whenSlash_thenCCCheckinClearToolCommandIsCleartool() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setClearToolDir("/");

    // Assert
    assertEquals("/cleartool", ccCheckin.getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#getClearToolCommand()}.
   * <ul>
   *   <li>Given {@link CCCheckin} (default constructor) ClearToolDir is {@code /}.</li>
   *   <li>Then return {@code /cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getClearToolCommand()}
   */
  @Test
  public void testGetClearToolCommand_givenCCCheckinClearToolDirIsSlash_thenReturnCleartool() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();
    ccCheckin.setClearToolDir("/");

    // Act and Assert
    assertEquals("/cleartool", ccCheckin.getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#getClearToolCommand()}.
   * <ul>
   *   <li>Given {@link CCCheckin} (default constructor).</li>
   *   <li>Then return {@code cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getClearToolCommand()}
   */
  @Test
  public void testGetClearToolCommand_givenCCCheckin_thenReturnCleartool() {
    // Arrange, Act and Assert
    assertEquals("cleartool", (new CCCheckin()).getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#getClearToolCommand()}.
   * <ul>
   *   <li>Then return {@code cleartool/cleartool}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getClearToolCommand()}
   */
  @Test
  public void testGetClearToolCommand_thenReturnCleartoolCleartool() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();
    ccCheckin.setClearToolDir("cleartool");

    // Act and Assert
    assertEquals("cleartool/cleartool", ccCheckin.getClearToolCommand());
  }

  /**
   * Test {@link ClearCase#setViewPath(String)}.
   * <p>
   * Method under test: {@link ClearCase#setViewPath(String)}
   */
  @Test
  public void testSetViewPath() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setViewPath("View Path");

    // Assert
    assertEquals("View Path", ccCheckin.getViewPath());
    assertEquals("View Path", ccCheckin.getViewPathBasename());
  }

  /**
   * Test {@link ClearCase#getViewPath()}.
   * <p>
   * Method under test: {@link ClearCase#getViewPath()}
   */
  @Test
  public void testGetViewPath() {
    // Arrange, Act and Assert
    assertNull((new CCCheckin()).getViewPath());
  }

  /**
   * Test {@link ClearCase#getViewPathBasename()}.
   * <ul>
   *   <li>Given {@link CCCheckin} (default constructor) ViewPath is {@code View Path}.</li>
   *   <li>Then return {@code View Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getViewPathBasename()}
   */
  @Test
  public void testGetViewPathBasename_givenCCCheckinViewPathIsViewPath_thenReturnViewPath() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();
    ccCheckin.setViewPath("View Path");

    // Act and Assert
    assertEquals("View Path", ccCheckin.getViewPathBasename());
  }

  /**
   * Test {@link ClearCase#setObjSelect(String)}.
   * <p>
   * Method under test: {@link ClearCase#setObjSelect(String)}
   */
  @Test
  public void testSetObjSelect() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setObjSelect("Obj Select");

    // Assert
    assertEquals("Obj Select", ccCheckin.getObjSelect());
  }

  /**
   * Test {@link ClearCase#getObjSelect()}.
   * <p>
   * Method under test: {@link ClearCase#getObjSelect()}
   */
  @Test
  public void testGetObjSelect() {
    // Arrange, Act and Assert
    assertNull((new CCCheckin()).getObjSelect());
  }

  /**
   * Test {@link ClearCase#getFailOnErr()}.
   * <ul>
   *   <li>Given {@link CCCheckin} (default constructor) FailOnErr is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getFailOnErr()}
   */
  @Test
  public void testGetFailOnErr_givenCCCheckinFailOnErrIsFalse_thenReturnFalse() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();
    ccCheckin.setFailOnErr(false);

    // Act and Assert
    assertFalse(ccCheckin.getFailOnErr());
  }

  /**
   * Test {@link ClearCase#getFailOnErr()}.
   * <ul>
   *   <li>Given {@link CCCheckin} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClearCase#getFailOnErr()}
   */
  @Test
  public void testGetFailOnErr_givenCCCheckin_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new CCCheckin()).getFailOnErr());
  }
}
