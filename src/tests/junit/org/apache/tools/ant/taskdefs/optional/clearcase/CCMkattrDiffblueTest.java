package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CCMkattrDiffblueTest {
  /**
   * Test {@link CCMkattr#execute()}.
   * <ul>
   *   <li>Given {@link CCMkattr} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMkattr#execute()}
   */
  @Test
  public void testExecute_givenCCMkattr_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CCMkattr()).execute());
  }

  /**
   * Test {@link CCMkattr#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMkattr#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    CCMkattr ccMkattr = new CCMkattr();
    ccMkattr.setTypeName("Required attribute TypeName not specified");

    // Act and Assert
    assertThrows(BuildException.class, () -> ccMkattr.execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMkattr#setComment(String)}
   *   <li>{@link CCMkattr#setCommentFile(String)}
   *   <li>{@link CCMkattr#setRecurse(boolean)}
   *   <li>{@link CCMkattr#setReplace(boolean)}
   *   <li>{@link CCMkattr#setTypeName(String)}
   *   <li>{@link CCMkattr#setTypeValue(String)}
   *   <li>{@link CCMkattr#setVersion(String)}
   *   <li>{@link CCMkattr#getComment()}
   *   <li>{@link CCMkattr#getCommentFile()}
   *   <li>{@link CCMkattr#getRecurse()}
   *   <li>{@link CCMkattr#getReplace()}
   *   <li>{@link CCMkattr#getTypeName()}
   *   <li>{@link CCMkattr#getTypeValue()}
   *   <li>{@link CCMkattr#getVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMkattr ccMkattr = new CCMkattr();

    // Act
    ccMkattr.setComment("Comment");
    ccMkattr.setCommentFile("Cfile");
    ccMkattr.setRecurse(true);
    ccMkattr.setReplace(true);
    ccMkattr.setTypeName("Tn");
    ccMkattr.setTypeValue("Tv");
    ccMkattr.setVersion("1.0.2");
    String actualComment = ccMkattr.getComment();
    String actualCommentFile = ccMkattr.getCommentFile();
    boolean actualRecurse = ccMkattr.getRecurse();
    boolean actualReplace = ccMkattr.getReplace();
    String actualTypeName = ccMkattr.getTypeName();
    String actualTypeValue = ccMkattr.getTypeValue();

    // Assert
    assertEquals("1.0.2", ccMkattr.getVersion());
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Tn", actualTypeName);
    assertEquals("Tv", actualTypeValue);
    assertTrue(actualRecurse);
    assertTrue(actualReplace);
  }

  /**
   * Test new {@link CCMkattr} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMkattr}
   */
  @Test
  public void testNewCCMkattr() {
    // Arrange and Act
    CCMkattr actualCcMkattr = new CCMkattr();

    // Assert
    assertEquals("cleartool", actualCcMkattr.getClearToolCommand());
    assertNull(actualCcMkattr.getDescription());
    assertNull(actualCcMkattr.getTaskName());
    assertNull(actualCcMkattr.getTaskType());
    assertNull(actualCcMkattr.getComment());
    assertNull(actualCcMkattr.getCommentFile());
    assertNull(actualCcMkattr.getTypeName());
    assertNull(actualCcMkattr.getTypeValue());
    assertNull(actualCcMkattr.getVersion());
    assertNull(actualCcMkattr.getObjSelect());
    assertNull(actualCcMkattr.getViewPath());
    assertNull(actualCcMkattr.getProject());
    assertNull(actualCcMkattr.getOwningTarget());
    assertFalse(actualCcMkattr.getRecurse());
    assertFalse(actualCcMkattr.getReplace());
    assertTrue(actualCcMkattr.getFailOnErr());
  }
}
