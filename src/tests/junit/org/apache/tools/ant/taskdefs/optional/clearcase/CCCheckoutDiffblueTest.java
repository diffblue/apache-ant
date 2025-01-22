package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCCheckoutDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCCheckout#setBranch(String)}
   *   <li>{@link CCCheckout#setComment(String)}
   *   <li>{@link CCCheckout#setCommentFile(String)}
   *   <li>{@link CCCheckout#setNoData(boolean)}
   *   <li>{@link CCCheckout#setNoWarn(boolean)}
   *   <li>{@link CCCheckout#setNotco(boolean)}
   *   <li>{@link CCCheckout#setOut(String)}
   *   <li>{@link CCCheckout#setReserved(boolean)}
   *   <li>{@link CCCheckout#setVersion(boolean)}
   *   <li>{@link CCCheckout#getBranch()}
   *   <li>{@link CCCheckout#getComment()}
   *   <li>{@link CCCheckout#getCommentFile()}
   *   <li>{@link CCCheckout#getNoData()}
   *   <li>{@link CCCheckout#getNoWarn()}
   *   <li>{@link CCCheckout#getNotco()}
   *   <li>{@link CCCheckout#getOut()}
   *   <li>{@link CCCheckout#getReserved()}
   *   <li>{@link CCCheckout#getVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCCheckout ccCheckout = new CCCheckout();

    // Act
    ccCheckout.setBranch("janedoe/featurebranch");
    ccCheckout.setComment("Comment");
    ccCheckout.setCommentFile("Cfile");
    ccCheckout.setNoData(true);
    ccCheckout.setNoWarn(true);
    ccCheckout.setNotco(true);
    ccCheckout.setOut("Outf");
    ccCheckout.setReserved(true);
    ccCheckout.setVersion(true);
    String actualBranch = ccCheckout.getBranch();
    String actualComment = ccCheckout.getComment();
    String actualCommentFile = ccCheckout.getCommentFile();
    boolean actualNoData = ccCheckout.getNoData();
    boolean actualNoWarn = ccCheckout.getNoWarn();
    boolean actualNotco = ccCheckout.getNotco();
    String actualOut = ccCheckout.getOut();
    boolean actualReserved = ccCheckout.getReserved();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Outf", actualOut);
    assertEquals("janedoe/featurebranch", actualBranch);
    assertTrue(actualNoData);
    assertTrue(actualNoWarn);
    assertTrue(actualNotco);
    assertTrue(actualReserved);
    assertTrue(ccCheckout.getVersion());
  }

  /**
   * Test new {@link CCCheckout} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCCheckout}
   */
  @Test
  public void testNewCCCheckout() {
    // Arrange and Act
    CCCheckout actualCcCheckout = new CCCheckout();

    // Assert
    assertEquals("cleartool", actualCcCheckout.getClearToolCommand());
    assertNull(actualCcCheckout.getDescription());
    assertNull(actualCcCheckout.getTaskName());
    assertNull(actualCcCheckout.getTaskType());
    assertNull(actualCcCheckout.getBranch());
    assertNull(actualCcCheckout.getComment());
    assertNull(actualCcCheckout.getCommentFile());
    assertNull(actualCcCheckout.getOut());
    assertNull(actualCcCheckout.getObjSelect());
    assertNull(actualCcCheckout.getViewPath());
    assertNull(actualCcCheckout.getProject());
    assertNull(actualCcCheckout.getOwningTarget());
    assertFalse(actualCcCheckout.getNoData());
    assertFalse(actualCcCheckout.getNoWarn());
    assertFalse(actualCcCheckout.getVersion());
    assertTrue(actualCcCheckout.getNotco());
    assertTrue(actualCcCheckout.getReserved());
    assertTrue(actualCcCheckout.getFailOnErr());
  }
}
