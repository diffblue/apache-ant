package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ContinuusDiffblueTest {
  /**
   * Test {@link Continuus#getCcmAction()}.
   * <p>
   * Method under test: {@link Continuus#getCcmAction()}
   */
  @Test
  public void testGetCcmAction() {
    // Arrange, Act and Assert
    assertEquals("", (new CCMCheck()).getCcmAction());
  }

  /**
   * Test {@link Continuus#setCcmAction(String)}.
   * <p>
   * Method under test: {@link Continuus#setCcmAction(String)}
   */
  @Test
  public void testSetCcmAction() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();

    // Act
    ccmCheck.setCcmAction("foo");

    // Assert
    assertEquals("foo", ccmCheck.getCcmAction());
  }

  /**
   * Test {@link Continuus#setCcmDir(String)}.
   * <ul>
   *   <li>When {@code Dir}.</li>
   *   <li>Then {@link CCMCheck} (default constructor) CcmCommand is {@code Dir/ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#setCcmDir(String)}
   */
  @Test
  public void testSetCcmDir_whenDir_thenCCMCheckCcmCommandIsDirCcm() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();

    // Act
    ccmCheck.setCcmDir("Dir");

    // Assert
    assertEquals("Dir/ccm", ccmCheck.getCcmCommand());
  }

  /**
   * Test {@link Continuus#setCcmDir(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link CCMCheck} (default constructor) CcmCommand is {@code ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#setCcmDir(String)}
   */
  @Test
  public void testSetCcmDir_whenEmptyString_thenCCMCheckCcmCommandIsCcm() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();

    // Act
    ccmCheck.setCcmDir("");

    // Assert that nothing has changed
    assertEquals("ccm", ccmCheck.getCcmCommand());
  }

  /**
   * Test {@link Continuus#setCcmDir(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then {@link CCMCheck} (default constructor) CcmCommand is {@code /ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#setCcmDir(String)}
   */
  @Test
  public void testSetCcmDir_whenSlash_thenCCMCheckCcmCommandIsCcm() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();

    // Act
    ccmCheck.setCcmDir("/");

    // Assert
    assertEquals("/ccm", ccmCheck.getCcmCommand());
  }

  /**
   * Test {@link Continuus#getCcmCommand()}.
   * <ul>
   *   <li>Given {@link CCMCheck} (default constructor) CcmDir is {@code ccm}.</li>
   *   <li>Then return {@code ccm/ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#getCcmCommand()}
   */
  @Test
  public void testGetCcmCommand_givenCCMCheckCcmDirIsCcm_thenReturnCcmCcm() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setCcmDir("ccm");

    // Act and Assert
    assertEquals("ccm/ccm", ccmCheck.getCcmCommand());
  }

  /**
   * Test {@link Continuus#getCcmCommand()}.
   * <ul>
   *   <li>Given {@link CCMCheck} (default constructor) CcmDir is {@code /}.</li>
   *   <li>Then return {@code /ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#getCcmCommand()}
   */
  @Test
  public void testGetCcmCommand_givenCCMCheckCcmDirIsSlash_thenReturnCcm() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setCcmDir("/");

    // Act and Assert
    assertEquals("/ccm", ccmCheck.getCcmCommand());
  }

  /**
   * Test {@link Continuus#getCcmCommand()}.
   * <ul>
   *   <li>Given {@link CCMCheck} (default constructor).</li>
   *   <li>Then return {@code ccm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Continuus#getCcmCommand()}
   */
  @Test
  public void testGetCcmCommand_givenCCMCheck_thenReturnCcm() {
    // Arrange, Act and Assert
    assertEquals("ccm", (new CCMCheck()).getCcmCommand());
  }
}
