package org.apache.tools.mail;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class MailMessageDiffblueTest {
  /**
   * Test {@link MailMessage#sanitizeAddress(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MailMessage#sanitizeAddress(String)}
   */
  @Test
  public void testSanitizeAddress_whenFoo_thenReturnFoo() {
    // Arrange, Act and Assert
    assertEquals("foo", MailMessage.sanitizeAddress("foo"));
  }

  /**
   * Test {@link MailMessage#sanitizeAddress(String)}.
   * <ul>
   *   <li>When {@code foo>}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MailMessage#sanitizeAddress(String)}
   */
  @Test
  public void testSanitizeAddress_whenFoo_thenReturnFoo2() {
    // Arrange, Act and Assert
    assertEquals("foo", MailMessage.sanitizeAddress("foo>"));
  }

  /**
   * Test {@link MailMessage#sanitizeAddress(String)}.
   * <ul>
   *   <li>When {@code >}.</li>
   *   <li>Then return {@code >}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MailMessage#sanitizeAddress(String)}
   */
  @Test
  public void testSanitizeAddress_whenGreaterThanSign_thenReturnGreaterThanSign() {
    // Arrange, Act and Assert
    assertEquals(">", MailMessage.sanitizeAddress(">"));
  }
}
